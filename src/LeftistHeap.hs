module LeftistHeap where

import Heap

data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a) deriving Show

instance Heap LeftistHeap where
    -- empty :: Ord a => LeftistHeap a
    empty = undefined

    -- isEmpty :: Ord a => LeftistHeap a -> Bool
    isEmpty = undefined

    -- insert :: Ord a => a -> LeftistHeap a -> LeftistHeap a
    insert = undefined

    -- merge :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
    merge = undefined

    -- findMin :: Ord a => LeftistHeap a -> Maybe a
    findMin = undefined

    -- deleteMin :: Ord a => LeftistHeap a -> Maybe (LeftistHeap a)
    deleteMin = undefined
