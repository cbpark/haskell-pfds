module Heap.LeftistHeap (LeftistHeap, fromList) where

import Heap

data LeftistHeap a = E | T Int a !(LeftistHeap a) !(LeftistHeap a) deriving Show

rank :: LeftistHeap a -> Int
rank E           = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x a b = if rank a >= rank b
              then T (rank b + 1) x a b
              else T (rank a + 1) x b a

instance Heap LeftistHeap where
    -- empty :: Ord a => LeftistHeap a
    empty = E

    -- isEmpty :: Ord a => LeftistHeap a -> Bool
    isEmpty E = True
    isEmpty _ = False

    -- insert :: Ord a => a -> LeftistHeap a -> LeftistHeap a
    -- insert x = merge (T 1 x E E)
    {- Exercise 3.2 -}
    insert x E = T 1 x E E
    insert x h@(T _ y a b) = if x <= y
                             then makeT x E h
                             else makeT y a (insert x b)

    -- merge :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
    merge h                E                = h
    merge E                h                = h
    merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) = if x <= y
                                              then makeT x a1 (merge b1 h2)
                                              else makeT y a2 (merge h1 b2)

    -- findMin :: Ord a => LeftistHeap a -> Maybe a
    findMin E           = Nothing
    findMin (T _ x _ _) = Just x

    -- deleteMin :: Ord a => LeftistHeap a -> Maybe (LeftistHeap a)
    deleteMin E           = Nothing
    deleteMin (T _ _ a b) = Just (merge a b)

    {- Exercise 3.3 -}
    -- |
    -- >>> fromList [1,3,9,10]
    -- T 2 1 (T 1 3 E E) (T 1 9 (T 1 10 E E) E)
    --
    -- >>> fromList [4,8,10,9,1,3,5,6,11]
    -- T 2 1 (T 2 4 (T 2 5 (T 1 6 E E) (T 1 9 (T 1 10 E E) E)) (T 1 8 E E)) (T 1 3 (T 1 11 E E) E)
    -- fromList :: Ord a => [a] -> LeftistHeap a
    -- fromList = foldl (flip insert) empty
    fromList []  = E
    fromList xs0 = mergeLoop (fmap (\x -> T 1 x E E) xs0)
      where
        mergeLoop [x] = x
        mergeLoop xs  = mergeLoop (mergePairs xs)

        mergePairs []         = []
        mergePairs [x]        = [x]
        mergePairs (x1:x2:xs) = merge x1 x2 : mergePairs xs
