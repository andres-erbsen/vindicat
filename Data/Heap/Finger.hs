{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | A min-max-priority queue implemented with ordered sequence: finger trees.
module Data.Heap.Finger
    ( OrdSeq
    , Heap
    , empty
    , null
    , insert
    , elem
    , extractMax
    , extractMin
    , increaseKey
    , decreaseKey
    , fromList
    , Data.Heap.Finger.fmap' )
    where

import Prelude hiding ( foldr, elem, null )
import Data.Monoid
import Data.FingerTree hiding ( empty, null, fromList)
import Data.Foldable (foldr)
import qualified Data.Foldable as FO
import qualified Data.FingerTree as FT

data Key a = NoKey | Key a deriving (Eq, Ord)

instance Monoid (Key a) where
    mempty = NoKey
    mappend k NoKey = k
    mappend _ k = k

-- | Info kept for each node in the tree.
type Info k a = (k,a)

instance Measured (Key k) (Info k a) where
    measure = Key . fst

-- descending ordered
newtype OrdSeq k a = OrdSeq (FingerTree (Key k) (Info k a))
    deriving Show

type Heap k a = OrdSeq k a

empty :: (Ord k) => Heap k a
empty = OrdSeq FT.empty

null :: (Ord k) => Heap k a -> Bool
null (OrdSeq t) = FT.null t

insert :: (Ord k) => k -> a -> Heap k a -> Heap k a
insert k v (OrdSeq xs) = OrdSeq (l >< (info <| r)) where 
    (l, r) = split (<= measure info) xs 
    info = (k, v)

-- | Extract the value whose key is maximum.  If more than one key has maximum
-- value, an arbitrary one is chosen.
--
-- Precondition: The heap is non-empty.
extractMax :: (Ord k) => Heap k a -> (k, a, Heap k a)
extractMax (OrdSeq s) = (k, v, OrdSeq s')
    where (k,v) :< s' = viewl s

extractMin :: (Ord k) => Heap k a -> (k, a, Heap k a)
extractMin (OrdSeq s) = (k, v, OrdSeq s')
    where s' :> (k,v) = viewr s

-- | @increaseKey old newKey h@.
--
-- Precondition: The old key must be in the heap, and the new key is no
-- smaller than the old.
increaseKey :: (Ord k, Eq a) =>
            Info k a              -- ^ old
            -> k                  -- ^ new
            -> Heap k a
            -> Heap k a
increaseKey oldInfo@(key,v) newKey (OrdSeq t) = OrdSeq (l' >< eqs' >< r')
    where (l, r)    = split (<= measure oldInfo) t
          (eqs, r') = split (< measure oldInfo) r
          eqs' = foldr (\i t' -> if i == oldInfo then t' else i <| t')
                 FT.empty eqs
          (OrdSeq l') = insert newKey v (OrdSeq l)

-- | @decreaseKey old newKey h@.
--
-- Precondition: The old key must be in the heap, and the new key is no
-- greater than the old.
decreaseKey :: (Ord k, Eq a) =>
            Info k a              -- ^ old
            -> k                  -- ^ new
            -> Heap k a
            -> Heap k a
decreaseKey oldInfo@(key,v) newKey (OrdSeq t) = OrdSeq (l' >< eqs' >< r')
    where (l, r)    = split (> measure oldInfo) t
          (l', eqs) = split (>= measure oldInfo) l
          eqs' = foldr (\i t' -> if i == oldInfo then t' else i <| t')
                 FT.empty eqs
          (OrdSeq r') = insert newKey v (OrdSeq r)

elem :: (Ord k, Eq a) => Info k a -> Heap k a -> Bool
x `elem` (OrdSeq t) = x `FO.elem` t

fromList :: (Ord k) => [Info k a] -> Heap k a
fromList = foldr (\(k,v) -> insert k v) empty


------------------------------------------------------------------------
-- Helpers

fmap' f (OrdSeq t) = OrdSeq $ FT.fmap' f t
