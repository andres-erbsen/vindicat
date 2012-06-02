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
  put (Mac a b c d e f) = mapM_ putWord8 [a,b,c,d,e,f]
  get = Mac <$> get <*> get <*> get <*> get <*> get <*> get

data EthernetFrame = EthernetFrame
  { etherDest   :: {-# UNPACK #-} !Mac
  , etherSource :: {-# UNPACK #-} !Mac
  , etherType   :: {-# UNPACK #-} !Word16
  , etherData   :: ByteString
  } deriving (Eq,Show)

instance Serialize EthernetFrame where
  put  (EthernetFrame s d t da) = put s >> put d >> put t >> putByteString da
  get = EthernetFrame <$> get <*> get <*> get <*> (getBytes =<< remaining)


