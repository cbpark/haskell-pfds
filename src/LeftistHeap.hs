module LeftistHeap where

import Heap

data LeftistHeap a = E | T Int a !(LeftistHeap a) !(LeftistHeap a) deriving Show

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

rank :: LeftistHeap a -> Int
rank E           = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x a b = if rank a >= rank b
              then T (rank b + 1) x a b
              else T (rank a + 1) x b a

{- Exercise 3.3 -}
fromList :: Ord a => [a] -> LeftistHeap a
-- fromList xs = foldl merge E (fmap (\x -> T 1 x E E) xs)
fromList []  = E
fromList xs0 = mergeLoop (fmap (\x -> T 1 x E E) xs0)
  where
    mergeLoop [x] = x
    mergeLoop xs  = mergeLoop (mergePairs xs)

    mergePairs []         = []
    mergePairs [x]        = [x]
    mergePairs (x1:x2:xs) = merge x1 x2 : mergePairs xs
