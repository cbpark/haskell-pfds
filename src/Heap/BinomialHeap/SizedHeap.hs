-- | Exercise 6.5
--
-- Restore the O(1) running time of isEmpty by explicitly maintaining the
-- size of every heap

module Heap.BinomialHeap.SizedHeap (SizedHeap) where

import Heap
import Heap.BinomialHeap

newtype SizedHeap a = SH (Int, BinomialHeap a) deriving Show

instance Heap SizedHeap where
    -- empty     :: Ord a => SizedHeap a
    empty = SH (0, empty)

    -- isEmpty   :: Ord a => SizedHeap a -> Bool
    isEmpty (SH (s, _)) = s == 0

    -- insert    :: Ord a => a -> SizedHeap a -> SizedHeap a
    insert x (SH (s, h)) = SH (s + 1, insert x h)

    -- merge     :: Ord a => SizedHeap a -> SizedHeap a -> SizedHeap a
    merge (SH (s1, h1)) (SH (s2, h2)) = SH (s1 + s2, merge h1 h2)

    -- findMin   :: Ord a => SizedHeap a -> Maybe a
    findMin (SH (_, h)) = findMin h

    -- deleteMin :: Ord a => SizedHeap a -> Maybe (SizedHeap a)
    deleteMin (SH (0, _)) = Nothing
    deleteMin (SH (s, h)) = do h' <- deleteMin h
                               return (SH (s - 1, h'))
