module Data.Ethernet (
    Mac(..)
  , EthernetFrame(..)
) where

import Data.Word
import Data.ByteString
import Control.Applicative
import Data.Serialize

data Mac = Mac
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  {-# UNPACK #-} !Word8
  deriving (Eq, Ord, Show)

instance Serialize Mac where
  put (Mac a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get = Mac <$> get <*> get <*> get <*> get <*> get <*> get

data EthernetFrame = EthernetFrame
  { etherDest   :: {-# UNPACK #-} !Mac
  , etherSource :: {-# UNPACK #-} !Mac
  , etherType   :: {-# UNPACK #-} !Word16
  , etherData   :: ByteString
  } deriving (Eq,Show)

instance Serialize EthernetFrame where
  put  (EthernetFrame d s t da) = put d >> put s >> put t >> putByteString da
  get = EthernetFrame <$> get <*> get <*> get <*> (getBytes =<< remaining)


