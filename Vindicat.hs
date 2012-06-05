module Vindicat (
    PubKey(..)
  , DeviceProperty(..)
  , Signature(..)
  , Device(..)
  , mkDevice
  , naclSign
  , isNaclKey
  , isHWAddr
  , isNick
  , verify
) where

import Data.Serialize
import Data.Word
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Monad
import Control.Applicative

import Crypto.NaCl.Key
import qualified Crypto.NaCl.Sign as NaCl
import Crypto.NaCl.Encrypt.PublicKey

import Data.Ethernet

-- FancyKey is code bloat, but let's leave it for now
data PubKey = NaclKey PublicKey
            | FancyKey ByteString ByteString
            | UnknownPubKey
    deriving (Show, Eq, Ord)

isNaclKey (NaclKey _) = True
isNaclKey _ = False

unNaclKey (NaclKey k) = k

instance Serialize PubKey where
  -- | Word8: constructor | Word16be: length | data...
  put (NaclKey (PublicKey k)) = do
    putWord8 0x01
    putWord16be (fromIntegral $ B.length k) >> putByteString k
  put (FancyKey name k) = do
  -- | Word8: constructor | Word16be: length | Word8: namelen | name | data...
    putWord8 0xFF
    putWord16be . fromIntegral $ 1 + B.length name + B.length k
    putWord8 . fromIntegral $ B.length name
    putByteString name
    putByteString k -- substract one and namelen from len for length of the key
  put UnknownPubKey = error "UnknownPubKey can not be serialized"
  
  get = do
    tag <- getWord8
    len <- fromIntegral <$> getWord16be :: Get Int
    case tag of
      0x01 -> do
        k <- getBytes $ fromIntegral len
        return $ NaclKey $ PublicKey k
      0xFF -> do
        namelen <- fromIntegral <$> getWord8 :: Get Int
        name <- getBytes namelen
        k <- getBytes (len-namelen-1)
        return $ FancyKey name k
      _ -> skip len >> return UnknownPubKey

data DeviceProperty = HWAddr Mac
                    | Nick ByteString
                    | UnknownDeviceProperty
    deriving (Show, Eq, Ord)

isHWAddr (HWAddr _) = True
isHWAddr _ = False
isNick   (Nick _)   = True
isNick   _ = False

unHWAddr (HWAddr mac) = mac
unNick (Nick s) = s

instance Serialize DeviceProperty where
  -- | Word8: constructor | Word16be: length | data...
  put (HWAddr mac) = putWord8 0x01 >> putWord16be 6 >> put mac
  put (Nick nick) = putWord8 0x02 >> putWord16be (fromIntegral (B.length nick)) >> putByteString nick
  put UnknownDeviceProperty = error "UnknownDeviceProperty can not be serialized"
  
  get = do
    tag <- getWord8
    len <- fromIntegral <$> getWord16be
    case tag of
      0x01 -> HWAddr <$> get
      0x02 -> Nick <$> getBytes len
      _    -> skip len >> return UnknownDeviceProperty

data Signature = NaclSignature ByteString -- 64 bytes
               | UnknownSignature
               deriving (Eq, Ord, Show)

instance Serialize Signature where
  put (NaclSignature s) = putWord8 0x01 >> putWord16be 64 >> putByteString s
  put UnknownSignature = error "UnknownSignature can not be serialized"
  get = do
    tag <- getWord8
    len <- fromIntegral <$> getWord16be :: Get Int
    case tag of
      0x01 -> NaclSignature <$> getBytes len
      _    -> return UnknownSignature

-- TODO: test signing and verification!
naclSign :: SecretKey -> ByteString -> Signature
naclSign sk msg = NaclSignature $ B.take 64 $ NaCl.sign sk msg
-- TODO: verify: signature is in first 64 bytes, so we can dropm the following copy of the message

verify :: ByteString -> PubKey -> Signature -> Bool
verify msg (NaclKey pk) (NaclSignature sig)
  = NaCl.verify pk (sig `B.append` msg) == Just msg
verify _ _ _ = False

data Device = Device
  { deviceNaclKey :: Maybe PublicKey
  , deviceMac    :: Maybe Mac
  , deviceNick   :: Maybe ByteString
  , deviceToBS   :: ByteString
  } deriving (Show, Eq, Ord)


instance Serialize Device where
  put = putByteString . deviceToBS
  -- UP NEXT: a piece of ugly code
  -- | Word8: No keys | keys... | Word8 No props | properties... | signatures...
  get = do
    totalbytes <- remaining -- the length of bytestring describing device
    rawdevice  <- lookAhead $ getBytes totalbytes -- bytestring describing device
    nokeys     <- fromIntegral <$> getWord8 :: Get Int -- number of public keys
    keys       <- replicateM nokeys get     :: Get [PubKey]
    noprops    <- fromIntegral <$> getWord8 :: Get Int -- number of device properties
    props      <- replicateM noprops get    :: Get [DeviceProperty]
    sigsbytes  <- remaining -- the length of bytestring that includes signatures
    sigs       <- replicateM nokeys get     :: Get [Signature] -- signatures, one per public key
    -- information about device EXCLUDING signatures
    let deviceinfo = B.take (totalbytes - sigsbytes) rawdevice
    -- list of verified key which signatures we could successfully verify
    let okKeys = map fst . filter (uncurry $ verify deviceinfo) $ zip keys sigs
    -- let othersigs = drop (length keys) signatures -- WOT?...
    let naclkey = unNaclKey <$> find isNaclKey okKeys -- (first) verified NaCl public key
    let mac     = unHWAddr  <$> find isHWAddr  props  -- (first) mac address
    let nick    = unNick    <$> find isNick    props  -- (first) nickname for device
    return $ Device naclkey mac nick rawdevice

mkDevice :: KeyPair -> Mac -> ByteString -> Device
mkDevice (pk,sk) mac nick
 = Device (Just pk) (Just mac) (Just nick) rawdevice where
  rawdevice = deviceinfo `B.append` (encode $ naclSign sk deviceinfo)
  deviceinfo = runPut $ do
    putWord8 1 >> put (NaclKey pk)
    putWord8 2 >> put (HWAddr mac) >> put (Nick nick)
