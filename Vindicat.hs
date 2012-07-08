{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
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
  , acceptLink
  , naclSign
  , verify
) where

import Data.DeriveTH

import Data.List (find,intersect)
import Data.Maybe
import Data.Serialize
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
$(  derive makeSerialize ''Signature )
$(  derive makeIs        ''Signature )

verify :: ByteString -> PubKey -> Signature -> Bool
verify msg (NaClKey pk) (NaClSignature sig)
 = NaCl.verify pk (sig `B.append` msg) == Just msg
verify _ _ _ = False

naclSign :: NaCl.SecretKey -> ByteString -> Signature
naclSign sk xs = NaClSignature $ NaCl.sign' sk xs
-- TODO: verify: signature is in first 64 bytes, so we can dropm the following copy of the message

type Acceptance = (PubKey, Signature) -- TODO: implement vouching

{- {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ Device }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} -}

data DeviceProperty = Nick {unNick :: ByteString}
                    | DeviceTime {unDeviceTime :: TAI64}
    deriving (Show, Eq, Ord)
$(  derive makeSerialize ''DeviceProperty )
$(  derive makeIs        ''DeviceProperty )

data Device = Device
  { deviceFirstKey :: Maybe PubKey
  , deviceNaClKey  :: Maybe NaCl.PublicKey
  , deviceNick     :: Maybe ByteString
  , deviceToBS     :: ByteString
  -- local fields (not serialised)
  , deviceGraphId  :: Maybe Int
  } deriving (Show, Ord)
device = Device Nothing Nothing Nothing "" Nothing

deviceKeys :: Device -> [PubKey]
deviceKeys dev = catMaybes [NaClKey <$> deviceNaClKey dev]

instance Eq Device where -- equivelence, not equality: the same real device
  a == b = not . null $ deviceKeys a `intersect` deviceKeys b

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
    putWord8 0  -- no accepts
  deviceinfo = runPut $ do
    putWord8 1 >> putWithLength put (NaClKey pk)
    putWord8 1 >> putWithLength put (Nick nick)


{-
-- | Device in wire format. Specification as Haskell code. All lists are to be
-- serialised with putNetList and getNetList
data NetDevice = NetDevice 
  { netDevPubkeys :: [PubKey]
  , netDevProps   :: [DeviceProperty]
  , netDevSig     :: [Signature]
  , netDevAccepts :: [Acceptance]
  }
-}

instance Serialize Device where
  put = putByteString . deviceToBS
  get = do
    totalbytes <- remaining
    rawdevice  <- lookAhead $ getBytes totalbytes -- original signed Device
    keys    <- getNetListOf get
    props   <- getNetListOf get
    sigsbytes  <- remaining -- end of device info, start of verification info
    sigs    <- getNetListOf get
    accepts <- getNetListOf get :: Get [Acceptance]
    let deviceinfo = B.take (totalbytes - sigsbytes) rawdevice -- shva :P
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

data LinkProperty = DeadLink
                  | LinkTime {unLinkTime :: TAI64}
                  deriving (Eq, Ord, Show)
$(  derive makeSerialize ''LinkProperty )
$(  derive makeIs        ''LinkProperty )

data Link = Link -- TODO: add efficency measures
  { linkLeftEnd  :: Device
  , linkRightEnd :: Device
  , linkDead     :: Bool
  , linkTime     :: Maybe TAI64
  , linkToBS     :: ByteString
  } deriving (Eq, Ord, Show)
link = Link device device True (Just $ TAI64 0x0) ""

{-
-- | Link in wire format. Specification as Haskell code. All lists are to be
-- serialised with *NetList, signatures using *WithLength
newtype NetLink = NetLink 
   { netLinkLeftDev  :: Device 
   , netLinkRightDev :: Device 
   , netLinkProps    :: [LinkProperty]
   , netLinkLeftSig  :: Signature -- with first  devices first key
   , netLinkRightSig :: Signature -- with second devices first key
   , netLinkAccepts  :: [Acceptance]
   }
-}

instance Serialize Link where
  put = putByteString . linkToBS
  get = do
    totalbytes <- remaining
    rawlink  <- lookAhead $ getBytes totalbytes -- original signed Link
    (left,right) <- getTwoOf get get       :: Get (Device, Device)
    props        <- getNetListOf get       :: Get [LinkProperty]
    sigsbytes  <- remaining -- end of device info, start of verification info
    mb_leftSig   <- getWithLength get      :: Get (Maybe Signature)
    mb_rightSig  <- getWithLength get      :: Get (Maybe Signature)
    accepts            <- getNetListOf get :: Get [Acceptance]
    let linkinfo = B.take (totalbytes - sigsbytes) rawlink -- the signed part
    case ret left right props mb_leftSig mb_rightSig accepts linkinfo rawlink of
      Just l -> return l
      Nothing -> fail "Bad signatures"
    where
    ret left right props mb_leftSig mb_rightSig accepts linkinfo rawlink = do
      leftKey  <- deviceFirstKey left
      rightKey <- deviceFirstKey right
      leftSig  <- mb_leftSig
      rightSig <- mb_rightSig
      let dead = DeadLink `elem` props
      if (verify linkinfo leftKey leftSig && verify linkinfo rightKey rightSig
            || dead && verify linkinfo leftKey leftSig) then
        return link { linkLeftEnd  = left
                    , linkRightEnd = right
                    , linkDead     = dead
                    , linkTime     = unLinkTime <$> find isLinkTime props
                    , linkToBS     = rawlink
                    }
      else fail "Bad signatures"

{- {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ LinkHalf }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}} -}

data LinkHalf = LinkHalf
  { linkHalfLeftEnd  :: Device
  , linkHalfRightEnd :: Device
  , linkHalfProps    :: [LinkProperty]
  , linkHalfInfo     :: ByteString
  , linkHalfSig      :: Signature
  } deriving (Eq, Ord, Show)
linkhalf = LinkHalf device device [] "" (NaClSignature "")

instance Serialize LinkHalf where
  put lh = putByteString (linkHalfInfo lh) >> putWithLength put (linkHalfSig lh)
  get = do
    totalbytes <- remaining
    rawlink  <- lookAhead $ getBytes totalbytes -- original signed Link
    (left,right) <- getTwoOf get get       :: Get (Device, Device)
    props        <- getNetListOf get       :: Get [LinkProperty]
    sigsbytes  <- remaining -- end of device info, start of verification info
    mb_leftSig   <- getWithLength get      :: Get (Maybe Signature)
    let linkinfo = B.take (totalbytes - sigsbytes) rawlink -- the signed part
    case ret left right props mb_leftSig linkinfo of
      Just l -> return l
      Nothing -> fail "Bad signatures"
    where
    ret left right props mb_leftSig linkinfo = do
      leftKey  <- deviceFirstKey left
      leftSig  <- mb_leftSig
      if verify linkinfo leftKey leftSig then
        return linkhalf { linkHalfLeftEnd  = left
                        , linkHalfRightEnd = right
                        , linkHalfProps    = props
                        , linkHalfInfo     = linkinfo
                        , linkHalfSig      = leftSig
                        }
      else fail "Bad signatures"


mkLinkHalf :: NaCl.KeyPair -> Device -> Device -> [LinkProperty] -> LinkHalf
mkLinkHalf (pk,sk) left right props
 = linkhalf { linkHalfLeftEnd  = left
            , linkHalfRightEnd = right
            , linkHalfProps    = props
            , linkHalfInfo     = linkinfo
            , linkHalfSig      = sig
            } where
  sig = naclSign sk linkinfo
  linkinfo = runPut $ do
    put left
    put right
    putWord8 . fromIntegral . length $ props
    mapM_ (putWithLength put) props

acceptLink :: NaCl.KeyPair -> LinkHalf -> Link
acceptLink (pk,sk) lh
 = link { linkLeftEnd  = linkHalfLeftEnd  lh
        , linkRightEnd = linkHalfRightEnd lh
        , linkDead     = DeadLink `elem` props
        , linkTime     = unLinkTime <$> find isLinkTime props
        , linkToBS     = rawlink
        } where
  props = linkHalfProps lh
  rawlink = runPut $ do 
    putByteString linkinfo
    putWithLength put $ linkHalfSig lh
    putWithLength put $ naclSign sk linkinfo
    putWord8 0 -- no accepts
  linkinfo = linkHalfInfo lh

