module SafeSerialize (
    putWithLength
  , getWithLength
  , putNetListOf
  , getNetListOf
  , getWithRaw
) where

import Data.Serialize
import Data.Serialize.Put (Putter)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B

import qualified Crypto.NaCl.Key               as NaCl
import qualified Crypto.NaCl.Sign              as NaCl
import qualified Crypto.NaCl.Encrypt.PublicKey as NaCl

putWithLength :: Putter a -> Putter a
putWithLength m x = do
  let put_x = runPut $ m x
  putWord16be . fromIntegral $ B.length put_x
  putByteString put_x

getWithLength :: Get a -> Get (Maybe a)
getWithLength m = do
  put_x <- getBytes . fromIntegral =<< getWord16be
  case runGet m put_x of
    Right x -> return . Just $! x
    _ -> return Nothing -- this item is unknown to us

-- | Serialise a list shorter than 256 elements using one byte for number of 
-- elements and two big-endian bytes to write the len of each element before it
putNetListOf :: Putter a -> Putter [a]
putNetListOf m xs = do
  putWord8 . fromIntegral . length $ xs
  mapM_ (putWithLength m) xs

getNetListOf :: Get a -> Get [a]
getNetListOf m = go [] =<< getWord8
  where
  go xs 0 = return (reverse xs)
  go xs i = do
    m_x <- getWithLength m
    case m_x of
      Just x  -> x `seq` go (x:xs) (pred i)
      Nothing ->         go (  xs) (pred i) -- this element is unknown to us

getWithRaw :: Get a -> Get (a,ByteString)
getWithRaw m = do
  start <- remaining
  (got,end) <- lookAhead $ do
    got <- m
    end <- remaining
    return (got,end)
  raw <- getBytes (start - end)
  return (got, raw)
