{-# LANGUAGE TemplateHaskell #-}

module Message (
    TunnelID
  , Packet(..)
  , Envelope(..)
  , postcardE
  , naclLetterE
  , openEnvelope
) where

import Control.Monad
import Control.Applicative
import Data.Serialize
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Tagged(Tagged,untag)

import Crypto.NaCl.Key
import Crypto.NaCl.Nonce
import Crypto.NaCl.Encrypt.PublicKey

import Vindicat

instance Serialize PublicKey where
  put = put . unPublicKey
  get = PublicKey <$> get

instance Serialize PKNonce where
  put = putByteString . toBS
  get = do
    bytes <- getByteString sz
    case fromBS bytes of
      Nothing -> fail "Nonce fromBS returned Nothing"
      Just k  -> return k
    where sz = untag (size :: Tagged PKNonce Int)

type TunnelID = Word32

data Packet = DevicePack Device
            | LinkPack Link
            | LinkReqPack LinkHalf
            | FwdPack TunnelID ByteString
            | DataPacket ByteString
            -- UnknownPacket
            -- TODO: Request to create tunnel
            --       ("send packets with this Tunnel ID to this device
            --       and forward the rest of the packet you're reading now there too")
            -- TODO: Packet to collect data for routing
            --       (depends on addressing system?)
  deriving (Show, Eq, Ord)

instance Serialize Packet where
  put (DevicePack device)    = putWord8 0x01 >> put device
  put (LinkPack link)        = putWord8 0x02 >> put link
  put (LinkReqPack linkhalf) = putWord8 0x03 >> put linkhalf
  put (FwdPack tunnelid bs)  = putWord8 0x04 >> put tunnelid >> putByteString bs
  put (DataPacket packet)    = putWord8 0x05 >> putByteString packet
  -- put UnknownPacket          = error "What's the point of encoding UnknownPacket?"
  
  get = do
    tag <- getWord8
    case tag of
      0x01 -> DevicePack <$> get
      0x02 -> LinkPack <$> get
      0x03 -> LinkReqPack <$> get
      0x04 -> FwdPack <$> get <*> (remaining >>= getByteString)
      0x05 -> DataPacket <$> (remaining >>= getByteString)
      -- _    -> return UnknownPacket

data Envelope = Postcard                     ByteString
              | NaclLetter PKNonce PublicKey ByteString
              -- UnknownEnvelope
  deriving (Show, Eq)

instance Serialize Envelope where
  put (Postcard c)                  = putWord8 0x00 >> putByteString c
  put (NaclLetter nonce pubk c) = putWord8 0x01 >> put nonce >> put pubk >> putByteString c
  -- put UnknownEnvelope               = error "Why on earth are you encoding UnknownEnvelope?"
  
  get = do
    tag <- getWord8
    case tag of
      0x00 -> Postcard <$> (remaining >>= getByteString)
      0x01 -> NaclLetter <$> get <*> get <*> (remaining >>= getByteString)
      -- _    -> return UnknownEnvelope

postcardE :: (Serialize a) => a -> Envelope
postcardE = Postcard . encode

naclLetterE :: (Serialize a) => KeyPair -> PublicKey -> PKNonce -> a -> Envelope
naclLetterE (pk, sk) pk' n x = NaclLetter n pk $ encrypt n ss pk' sk
  where ss = encode x

openEnvelope :: (Serialize a) => KeyPair -> Envelope -> Either String a
openEnvelope _ (Postcard ss) = decode ss
openEnvelope (pk,sk) (NaclLetter n pk' c) =
  case decrypt n c pk' sk of
    Nothing -> Left "Decryption failed"
    Just x  -> decode x
