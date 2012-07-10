{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Vindicat (
    PubKey(..)
  , DeviceProperty(..)
  , Signature(..)
  , Device(..)
  , Medium(..)
  , LinkProperty(..)
  , Link(..)
  , mkDevice
  , deviceKeys
  , mkLinkRq
  , acceptLink
  , naclSign
  , verify
) where

import Data.DeriveTH

import Data.List (find,intersect)
import Data.Maybe
import Data.Serialize
import Data.Word
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Control.Applicative

import qualified  Crypto.NaCl.Key              as NaCl
import qualified Crypto.NaCl.Sign              as NaCl
import qualified Crypto.NaCl.Encrypt.PublicKey as NaCl

import Data.Time.TAI64
import SafeSerialize

instance Serialize NaCl.PublicKey where
  put = putByteString . NaCl.unPublicKey
  get = NaCl.PublicKey <$> getBytes 32 -- TODO: make NaCl typesafe :P

data PubKey = NaClKey {unNaClKey :: NaCl.PublicKey}
    deriving (Show, Eq, Ord)
$(  derive makeSerialize ''PubKey )
$(  derive makeIs        ''PubKey )

{- {{{{{{{{{{{{{{{{{{{{{{{{{{{{{ Verifications }}}}}}}}}}}}}}}}}}}}}}}}}}}}} -}

data Signature = NaClSignature {unNaClSignature :: ByteString} -- sign' -> 64byte
    deriving (Eq, Ord, Show)
$(  derive makeIs        ''Signature )

instance Serialize Signature where
  put (NaClSignature xs) = putWord8 0x01 >> putByteString xs
  get = getWord8 >>= \tag -> case tag of
    0x01 -> NaClSignature <$> getBytes 64
    _    -> fail "unknown constructor"
  

verify :: ByteString -> PubKey -> Signature -> Bool
verify msg (NaClKey pk) (NaClSignature sig)
 = NaCl.verify pk (sig `B.append` msg) == Just msg
verify _ _ _ = False

naclSign :: NaCl.SecretKey -> ByteString -> Signature
naclSign sk xs = NaClSignature $ NaCl.sign' sk xs
-- TODO: verify: signature is in first 64 bytes, so we can dropm the following copy of the message


{- {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ Device }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} -}

data DeviceProperty = Nick {unNick :: ByteString}
                    | DeviceTime {unDeviceTime :: TAI64}
    deriving (Show, Eq, Ord)
$(  derive makeIs        ''DeviceProperty )

instance Serialize DeviceProperty where
  put (Nick xs)      = putWord8 0x01 >> putByteString xs
  put (DeviceTime x) = putWord8 0x02 >> put x
  get = getWord8 >>= \tag -> case tag of
    0x01 -> Nick <$> (getBytes =<< remaining) -- ONLY in getWithLength
    0x02 -> DeviceTime <$> get
    _ -> fail "unknown constructor"

data Device = Device
  { deviceFirstKey :: Maybe PubKey
  , deviceNaClKey  :: Maybe NaCl.PublicKey
  , deviceNick     :: Maybe ByteString
  , deviceToBS     :: ByteString
  -- local fields (not serialised)
  , deviceGraphId  :: Maybe Int
  } deriving (Show, Ord,Eq)
device = Device Nothing Nothing Nothing "\NUL\NUL\NUL" Nothing

deviceKeys :: Device -> [PubKey]
deviceKeys dev = catMaybes [NaClKey <$> deviceNaClKey dev]

-- | Test whether two @Device@s represent the same physical device. Checks
-- whether a key (the intersection) has signed two devices as its own.
sameDev :: Device -> Device -> Bool
a `sameDev` b = not . null $ deviceKeys a `intersect` deviceKeys b

mkDevice :: NaCl.KeyPair -> ByteString -> Device
mkDevice (pk,sk) nick
 = device { deviceFirstKey = Just . NaClKey $ pk
          , deviceNaClKey  = Just pk
          , deviceNick     = Just nick
          , deviceToBS     = rawdevice
          } where
  rawdevice =  runPut $ do
    putByteString deviceinfo
    putNetListOf put [naclSign sk deviceinfo] -- sigs
  deviceinfo = runPut $ do
    putNetListOf put [NaClKey pk]
    putNetListOf put [Nick nick ]

{-
-- | Device in wire format. Specification as Haskell code. All lists are to be
-- serialised with putNetList and getNetList
data NetDevice = NetDevice 
  { netDevPubkeys :: [PubKey]
  , netDevProps   :: [DeviceProperty]
  , netDevSig     :: [Signature]
  }
-}

instance Serialize Device where
  put = putByteString . deviceToBS
  get = do
    ((keys,props,sigs,deviceinfo),rawdevice) <- getWithRaw $ do
      ((keys,props),deviceinfo) <- getWithRaw $ do
        keys    <- getNetListOf get
        props   <- getNetListOf get
        return (keys,props)
      sigs    <- getNetListOf get
      return (keys,props,sigs,deviceinfo)
    -- the keys for which a valid signature on this device was found
    let okKeys = map fst . filter (uncurry $ verify deviceinfo) $ zip keys sigs
    -- for each field take first item of the right type
    let mb_firstkey = if (listToMaybe keys) == (listToMaybe okKeys)
                        then listToMaybe keys
                        else Nothing
    let mb_nacl_PublicKey = unNaClKey <$> find isNaClKey okKeys
    let mb_nick           = unNick    <$> find isNick    props
    return device { deviceFirstKey = mb_firstkey
                  , deviceNaClKey  = mb_nacl_PublicKey
                  , deviceNick     = mb_nick
                  , deviceToBS     = rawdevice
                  }
    
{- {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ Link }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} -}

data Medium = Wifi | Cable | Optical | Pigeon
    deriving (Eq, Ord, Show)
$(  derive makeSerialize ''Medium )
$(  derive makeIs        ''Medium )

data LinkProperty = DeadLink
                  | LinkTime   {unLinkTime   :: TAI64}
                  | LinkIfaceL {unLinkIfaceL :: Word8}
                  | LinkIfaceR {unLinkIfaceR :: Word8}
                  | LinkMedium {unLinkMedium :: Medium}
    deriving (Eq, Ord, Show)
$(  derive makeSerialize ''LinkProperty )
$(  derive makeIs        ''LinkProperty )

data Link = Link -- TODO: add efficency measures
  { linkToBS      :: ByteString
  , linkRequestBS :: Maybe ByteString
  , linkLeftEnd   :: Device
  , linkRightEnd  :: Device
  , linkIsKnown   :: Bool
  , linkDead      :: Bool
  , linkTime      :: Maybe TAI64
  , linkMedium    :: Maybe Medium
  , linkIfaceL    :: Maybe Word8
  , linkIfaceR    :: Maybe Word8
  } deriving (Eq, Ord, Show)
link = Link "lXl" Nothing device device False False Nothing Nothing Nothing Nothing

-- | Test whether two @Link@s represent the same physical link. Checks
-- whether there are no conflicting properties (connection type, interface)
-- but ignores differences in efficency measures and timestamps.
sameLink :: Link -> Link -> Bool
a `sameLink` b = idents a == idents b where
  idents x = (linkLeftEnd x, linkRightEnd x, linkMedium x, 
              linkIfaceL x, linkIfaceR x)

setLinkProperty :: LinkProperty -> Link -> Link
setLinkProperty p l = case p of
  DeadLink     -> l {linkDead   = True}
  LinkTime   x -> l {linkTime   = Just x}
  LinkIfaceL x -> l {linkIfaceL = Just x}
  LinkIfaceR x -> l {linkIfaceR = Just x}
  LinkMedium x -> l {linkMedium = Just x}

setLinkProps :: Link -> [LinkProperty] -> Link
setLinkProps l = (foldr setLinkProperty) l . reverse

{-
-- | Link in wire format. Specification as Haskell code. All lists are to be
-- serialised with *NetList, signatures using *WithLength
newtype NetLink = NetLink 
   { netLinkLeftDev  :: Device 
   , netLinkRightDev :: Device 
   , netLinkProps    :: [LinkProperty]
   , netLinkLeftSigs  :: [Signature]
   , netLinkRightSigs  :: [Signature]
   }
-}

instance Serialize Link where
  put = putByteString . linkToBS
  get = do
    ((left,right,n,props,leftsigs,rightsigs,linkinfo),rawlink) <- getWithRaw $ do
      ((left,right,n,props),linkinfo) <- getWithRaw $ do
        (left,right) <- getTwoOf get get :: Get (Device, Device)
        n <- lookAhead $ getWord8
        props <- getNetListOf get :: Get [LinkProperty]
        return (left,right,n,props)
      leftsigs <- getNetListOf get :: Get [Signature]
      rightsigs <- getNetListOf get :: Get [Signature]
      return (left,right,n,props,leftsigs,rightsigs,linkinfo)
    let mb_leftSig  = listToMaybe leftsigs
    let mb_rightSig = listToMaybe rightsigs
    -- Did we understand all the properties? Won't sign ath otherwise.
    let isknown = length props == fromIntegral n
    -- Is this a link request?
    let isrq = not . isJust $ mb_rightSig
    -- Either side can drop a link without the others permission
    let dead = DeadLink `elem` props
    let ret = link { linkToBS      = rawlink
                   , linkLeftEnd   = left
                   , linkRightEnd  = right
                   , linkRequestBS = if isrq then Just linkinfo else Nothing
                   , linkIsKnown   = isknown
                   } `setLinkProps` props -- first over last
    let check = verifymb linkinfo
    if check left mb_leftSig && (dead || isrq || check right mb_rightSig)
      then return ret
      else fail "not signed enough"
    where
    verifymb linkinfo dev mb_sig = False `fromMaybe` do
      key <- deviceFirstKey dev
      sig  <- mb_sig
      return $ verify linkinfo key sig

mkLinkRq :: NaCl.KeyPair -> Device -> Device -> [LinkProperty] -> Link
mkLinkRq (_,sk) left right props = link
  { linkToBS      = rawlink
  , linkLeftEnd   = left
  , linkRightEnd  = right
  , linkRequestBS = Just linkinfo
  , linkIsKnown   = True
  } `setLinkProps` props  where
  rawlink = runPut $ do
    putByteString linkinfo
    putNetListOf put [sig]
    putNetListOf put ([] :: [Signature])
  sig = naclSign sk linkinfo
  linkinfo = runPut $ do
    put left
    put right
    putWord8 . fromIntegral . length $ props
    mapM_ (putWithLength put) props


acceptLink :: NaCl.KeyPair -> Link -> Link
acceptLink (_,sk) link = link
  { linkToBS      = rawlink
  , linkRequestBS = Nothing
  } where
  -- drop the null list placeholder, add signature
  rawlink = runPut $ do
    putByteString . B.init . linkToBS $ link
    putNetListOf put [sig]
  sig = naclSign sk . fromJust . linkRequestBS $ link
