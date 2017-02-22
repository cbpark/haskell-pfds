module Heap.PairingHeap (PairingHeap) where

import Heap

data PairingHeap a = E | T a [PairingHeap a] deriving Show

mergePairs :: Ord a => [PairingHeap a] -> PairingHeap a
mergePairs []         = E
mergePairs [h]        = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

instance Heap PairingHeap where
    -- empty     :: Ord a => PairingHeap a
    empty = E

    -- isEmpty   :: Ord a => PairingHeap a -> Bool
    isEmpty E = True
    isEmpty _ = False

    -- insert    :: Ord a => a -> PairingHeap a -> PairingHeap a
    insert x = merge (T x [])

    -- merge     :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
    merge h            E            = h
    merge E            h            = h
    merge h1@(T x hs1) h2@(T y hs2) = if x < y
                                      then T x (h2:hs1)
                                      else T y (h1:hs2)

    -- findMin   :: Ord a => PairingHeap a -> Maybe a
    findMin E       = Nothing
    findMin (T x _) = Just x

    -- deleteMin :: Ord a => PairingHeap a -> Maybe (PairingHeap a)
    deleteMin E        = Nothing
    deleteMin (T _ hs) = Just (mergePairs hs)
