module Vindicat (
    PubKey(..)
  , DeviceProperty(..)
  , Signature(..)
  , Device(..)
  , LinkProperty(..)
  , Link(..)
  , LinkHalf(..)
  , mkDevice
  , deviceKeys
  , mkLinkHalf
  , deviceGraphId
  , acceptLink
  , naclSign
  , isNaclKey
  , isHWAddr
  , isNick
  , verify
) where

import Data.Serialize
import Data.Word
import Data.List
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Monad
import Control.Applicative

import Crypto.NaCl.Key
import qualified Crypto.NaCl.Sign as NaCl
import Crypto.NaCl.Encrypt.PublicKey

import Data.Ethernet
import Data.Time.TAI64

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
                    | DeviceTime TAI64
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
  put (HWAddr mac)   = putWord8 0x01 >> putWord16be 6 >> put mac
  put (Nick nick)    = putWord8 0x02 >> putWord16be (fromIntegral (B.length nick)) >> putByteString nick
  put (DeviceTime t) = putWord8 0x03 >> putWord16be 8 >> put t
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

naclSign :: SecretKey -> ByteString -> Signature
naclSign sk msg = NaclSignature $ B.take 64 $ NaCl.sign sk msg
-- TODO: verify: signature is in first 64 bytes, so we can dropm the following copy of the message

verify :: ByteString -> PubKey -> Signature -> Bool
verify msg (NaclKey pk) (NaclSignature sig)
  = NaCl.verify pk (sig `B.append` msg) == Just msg
verify _ _ _ = False



data Device = Device
  { deviceNaclKey :: Maybe PublicKey
  , deviceMac     :: Maybe Mac
  , deviceNick    :: Maybe ByteString
  , deviceToBS    :: ByteString
  -- local fields
  , deviceGraphId  :: Maybe Int
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
    let naclkey = unNaclKey <$> find isNaclKey okKeys -- (first) verified NaCl public key
    let mac     = unHWAddr  <$> find isHWAddr  props  -- (first) mac address
    let nick    = unNick    <$> find isNick    props  -- (first) nickname for device
    return $ Device naclkey mac nick rawdevice Nothing

mkDevice :: KeyPair -> Mac -> ByteString -> Device
mkDevice (pk,sk) mac nick
 = Device (Just pk) (Just mac) (Just nick) rawdevice Nothing where
  rawdevice = deviceinfo `B.append` (encode $ naclSign sk deviceinfo)
  deviceinfo = runPut $ do
    putWord8 1 >> put (NaclKey pk)
    putWord8 2 >> put (HWAddr mac) >> put (Nick nick)

deviceKeys dev = catMaybes [NaclKey <$> deviceNaclKey dev]


data LinkProperty = DeadLink
                  | UnknownLinkProperty
                  | LinkTime TAI64
                  deriving (Eq, Ord, Show)

instance Serialize LinkProperty where
  put DeadLink = putWord8 0x00 >> putWord16be 0
  put (LinkTime t) = putWord8 0x03 >> putWord16be 8 >> put t
  put UnknownLinkProperty = error "UnknownLinkProperty cannot be serialized"
  get = do
    tag <- getWord8
    len <- fromIntegral <$> getWord16be :: Get Int
    case tag of
      0x00 -> skip len >> return DeadLink
      _    -> skip len >> return UnknownLinkProperty


data Link = Link -- TODO: add efficency measures
  { linkLeftEnd  :: PubKey
  , linkRightEnd :: PubKey
  , linkDead     :: Bool
  , linkToBS     :: ByteString
  } deriving (Eq, Ord, Show)

instance Serialize Link where
  -- | leftPubKey | rightPubKey | Word8 No props | props... | Word8 No sigs | sigs...
  put = putByteString . linkToBS
  get = do
    totalbytes <- remaining
    rawlink    <- lookAhead $ getBytes totalbytes
    leftKey    <- get                       :: Get PubKey
    rightKey   <- get                       :: Get PubKey
    noprops    <- fromIntegral <$> getWord8 :: Get Int -- number of link properties
    props      <- replicateM noprops get    :: Get [LinkProperty]
    sigsbytes  <- remaining -- the length of bytestring that includes signatures
    nosigs     <- fromIntegral <$> getWord8 :: Get Int -- number of signatures
    signatures <- replicateM nosigs get    :: Get [Signature]
    let linkinfo = B.take (totalbytes - sigsbytes) rawlink -- the signed part
    case signatures of
      lS:rS:_ -> case verify linkinfo leftKey  lS &&
                      verify linkinfo rightKey rS of
                   True  -> return $ Link leftKey rightKey False rawlink
                   False -> fail "Signature verification failed"
      lS:[]   -> case DeadLink `elem` props && verify linkinfo leftKey lS of
                   True  -> return $ Link leftKey rightKey True rawlink
                   False -> fail "A link has to be a DeadLink to get done with one signature and that signature has to be valid"
      _       -> fail "Link has to be signed"


data LinkHalf = LinkHalf
  { linkHalfLeftEnd  :: PubKey
  , linkHalfRightEnd :: PubKey
  , linkHalfProps    :: [LinkProperty]
  , linkHalfInfo     :: ByteString
  , linkHalfSig      :: Signature
  } deriving (Eq, Ord, Show)

instance Serialize LinkHalf where
  -- | leftPubKey | rightPubKey | Word8 No props | props... | Word8 No sigs | sigs...
  put (LinkHalf _ _ _ inf sig) = putByteString inf >> putWord8 1 >> put sig
  get = do
    totalbytes <- remaining
    rawlink    <- lookAhead $ getBytes totalbytes
    leftKey    <- get                       :: Get PubKey
    rightKey   <- get                       :: Get PubKey
    noprops    <- fromIntegral <$> getWord8 :: Get Int -- number of link properties
    props      <- replicateM noprops get    :: Get [LinkProperty]
    sigsbytes  <- remaining -- the length of bytestring that includes signatures
    nosigs     <- fromIntegral <$> getWord8 :: Get Int -- number of signatures
    signatures <- replicateM nosigs get    :: Get [Signature]
    let linkinfo = B.take (totalbytes - sigsbytes) rawlink -- the signed part
    case signatures of
      [lS] -> case verify linkinfo leftKey lS of
                   True  -> return $ LinkHalf leftKey rightKey props linkinfo lS
                   False -> fail "Invalid signature"
      _    -> fail $ "LinkHalf needs exactly one (valid) signature but got " ++ show (length signatures) ++ " of the advertised " ++ show nosigs


mkLinkHalf :: KeyPair -> PubKey -> [LinkProperty] -> LinkHalf
mkLinkHalf (pk,sk) otherpk props
 = LinkHalf (NaclKey pk) otherpk props linkinfo sig where
  sig = naclSign sk linkinfo
  linkinfo = runPut $ do
    put (NaclKey pk)
    put otherpk
    putWord8 (fromIntegral $ length props) >> mapM_ put props

acceptLink :: KeyPair -> LinkHalf -> Link
acceptLink (pk,sk) (LinkHalf l r p linkinfo othersig)
 = Link l r (DeadLink `elem` p) rawlink where
  rawlink = linkinfo `B.append` sigs
  sigs = runPut (putWord8 2 >> put othersig >> put (naclSign sk linkinfo))



