{-# LANGUAGE StrictData #-}

module Heap.SplayHeap (SplayHeap) where

import Heap

data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a) deriving Show

-- bigger :: Ord a => a -> SplayHeap a -> SplayHeap a
-- bigger _     E         = E
-- bigger pivot (T a x b) =
--     if x <= pivot
--     then bigger pivot b
--     else case a of
--              E         -> T E x b
--              T a1 y a2 -> if y <= pivot
--                           then T (bigger pivot a2) x b
--                           else T (bigger pivot a1) y (T a2 x b)

{- Exercise 5.4 -}
-- smaller :: Ord a => a -> SplayHeap a -> SplayHeap a
-- smaller _     E         = E
-- smaller pivot (T a x b) =
--     if x > pivot
--     then smaller pivot a
--     else case b of
--              E         -> T a x E
--              T a1 y a2 -> if y > pivot
--                           then T a          x (smaller pivot a1)
--                           else T (T a x a1) y (smaller pivot a2)

partition :: Ord a => a -> SplayHeap a -> (SplayHeap a, SplayHeap a)
partition _     E           = (E, E)
partition pivot t@(T a x b) =
    if x <= pivot
    then case b of
             E         -> (t, E)
             T b1 y b2 -> if y <= pivot
                          then let (small, big) = partition pivot b2
                               in (T (T a x b) y small, big)
                          else let (small, big) = partition pivot b1
                               in (T a x small, T big y b2)
    else case a of
             E         -> (E, t)
             T a1 y a2 -> if y <= pivot
                          then let (small, big) = partition pivot a2
                               in (T a1 y small, T big x b)
                          else let (small, big) = partition pivot a1
                               in (small, T big y (T a2 x b))

instance Heap SplayHeap where
    -- empty     :: Ord a => SplayHeap a
    empty = E

    -- isEmpty   :: Ord a => SplayHeap a -> Bool
    isEmpty E = True
    isEmpty _ = False

    -- insert    :: Ord a => a -> SplayHeap a -> SplayHeap a
    -- insert x t = T (smaller x t) x (bigger x t)
    insert x t = T a x b
      where (a, b) = partition x t

    -- merge     :: Ord a => SplayHeap a -> SplayHeap a -> SplayHeap a
    merge E         t = t
    merge (T a x b) t = T (merge ta a) x (merge tb b)
      where (ta, tb) = partition x t

    -- findMin   :: Ord a => SplayHeap a -> Maybe a
    findMin E         = Nothing
    findMin (T E x _) = Just x
    findMin (T a _ _) = findMin a

    -- deleteMin :: Ord a => SplayHeap a -> Maybe (SplayHeap a)
    deleteMin E                 = Nothing
    deleteMin (T E _ b)         = Just b
    deleteMin (T (T E _ b) y c) = Just (T b y c)
    deleteMin (T (T a x b) y c) = do l <- deleteMin a
                                     return (T l x (T b y c))
