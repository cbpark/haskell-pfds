module Heap.LazyPairingHeap where

import Heap

data PairingHeap a = E | T a !(PairingHeap a) (PairingHeap a) deriving Show

link :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
link (T x E m) a = T x a m
link (T x b m) a = T x E (merge (merge a b) m)
link _         a = a

instance Heap PairingHeap where
    -- empty     :: Ord a => PairingHeap a
    empty = E

    -- isEmpty   :: Ord a => PairingHeap a -> Bool
    isEmpty E = True
    isEmpty _ = False

    -- insert    :: Ord a => a -> PairingHeap a -> PairingHeap a
    insert x = merge (T x E E)

    -- merge     :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
    merge a           E           = a
    merge E           b           = b
    merge a@(T x _ _) b@(T y _ _) = if x <= y then link a b else link b a

    -- findMin   :: Ord a => PairingHeap a -> Maybe a
    findMin E         = Nothing
    findMin (T x _ _) = Just x

    -- deleteMin :: Ord a => PairingHeap a -> Maybe (PairingHeap a)
    deleteMin E         = Nothing
    deleteMin (T _ a m) = Just (merge a m)
