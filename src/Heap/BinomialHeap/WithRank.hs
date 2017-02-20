-- | Exercise 3.6
--
-- Remove the rank annotations from each node and instead pair each tree
-- at the top-level with its rank.

module Heap.BinomialHeap.WithRank (BinomialHeap, fromList) where

import Heap

import Data.Foldable (foldl')

data Tree a = Node { root :: a, children :: [Tree a] }

instance Show a => Show (Tree a) where
    show (Node x ts) = "Node " ++ show x ++ " " ++ show ts

newtype BinomialHeap a = BH [(Int, Tree a)] deriving Show

link :: Ord a => Tree a -> Tree a -> Tree a
link n1@(Node x1 c1) n2@(Node x2 c2) =
    if x1 <= x2 then Node x1 (n2:c1) else Node x2 (n1:c2)

insTree :: Ord a => Int -> Tree a -> [(Int, Tree a)] -> [(Int, Tree a)]
insTree r n []                = [(r, n)]
insTree r n ts@((r', n'):ts') =
    if r < r' then (r, n):ts else insTree (r + 1) (link n n') ts'

mrg :: Ord a => [(Int, Tree a)] -> [(Int, Tree a)] -> [(Int, Tree a)]
mrg ts1                    []  = ts1
mrg []                     ts2 = ts2
mrg ts1@(t1@(r1, n1):ts1') ts2@(t2@(r2, n2):ts2')
    | r1 < r2 = t1 : mrg ts1' ts2
    | r1 > r2 = t2 : mrg ts1 ts2'
    | otherwise = insTree r1 (link n1 n2) (mrg ts1' ts2')

removeMinTree :: Ord a => [(Int, Tree a)] -> Maybe (Tree a, [(Int, Tree a)])
removeMinTree []            = Nothing
removeMinTree [(_, n)]      = return (n, [])
removeMinTree (t@(_, n):ts) = do
    (n', ts') <- removeMinTree ts
    return $ if root n < root n' then (n, ts) else (n', t : ts')

instance Heap BinomialHeap where
    -- empty :: Ord a => BinomialHeap a
    empty = BH []

    -- isEmpty :: Ord a => BinomialHeap a -> Bool
    isEmpty (BH ts) = null ts

    -- insert :: Ord a => a -> BinomialHeap a -> BinomialHeap a
    insert x (BH ts) = BH (insTree 0 (Node x []) ts)

    -- merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
    merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)

    -- findMin :: Ord a => BinomialHeap a -> Maybe a
    findMin (BH []) = Nothing
    findMin (BH ts) = Just $ minimum (fmap (root . snd) ts)

    -- deleteMin :: Ord a => BinomialHeap a -> Maybe (BinomialHeap a)
    deleteMin (BH ts) =
        case removeMinTree ts of
            Just (t, ts2) -> let ts1 = zip [0..] (reverse (children t))
                             in Just (BH (mrg ts1 ts2))
            _             -> Nothing

-- |
-- >>> fromList [1,2]
-- BH [(1,Node 1 [Node 2 []])]
--
-- >>> fromList [1,2,3,4,5,6,7,8]
-- BH [(3,Node 1 [Node 5 [Node 7 [Node 8 []],Node 6 []],Node 3 [Node 4 []],Node 2 []])]
fromList :: Ord a => [a] -> BinomialHeap a
fromList = foldl' (flip insert) empty
