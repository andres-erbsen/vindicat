module Data.ShortList (
    ShortList,
    mkShortList
) where

import Data.Serialize
import Control.Applicative

newtype ShortList a = ShortList [a]

mkShortList xs
 | length xs > 255 = error "ShortList can contain 0..255 items"
 | otherwise = ShortList xs

unShortList (ShortList xs) = xs

instance (Serialize a) => Serialize (ShortList a) where
  put = putShortListOf put
  get = getShortListOf get

putShortListOf :: Putter a -> Putter (ShortList a)
putShortListOf pa = go 0 (return ()) . unShortList
  where
  go n body []     = putWord8 n >> body
  go n body (x:xs) = n' `seq` go n' (body >> pa x) xs
    where n' = n + 1
{-# INLINE putShortListOf #-}

getShortListOf :: Get a -> Get (ShortList a)
getShortListOf m = ShortList <$> (go [] =<< getWord8)
  where
  go as 0 = return (reverse as)
  go as i = do x <- m
               x `seq` go (x:as) (i - 1)

