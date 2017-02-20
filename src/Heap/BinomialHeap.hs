module Heap.BinomialHeap (BinomialHeap (..), fromList) where

import Heap

import Data.Foldable (foldl')

-- data Tree a = Node Int a [Tree a] deriving Show
data Tree a = Node { rank :: Int, root :: a, children :: [Tree a] }

instance Show a => Show (Tree a) where
    show (Node r x ts) = "Node " ++ show r ++ " " ++ show x ++ " " ++ show ts

-- rank :: Tree a -> Int
-- rank (Node r _ _) = r

-- root :: Tree a -> a
-- root (Node _ x _) = x

newtype BinomialHeap a = BH { trees :: [Tree a] }

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
    if x1 <= x2 then Node (r + 1) x1 (t2:c1) else Node (r + 1) x2 (t1:c2)

insTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insTree t []          = [t]
insTree t ts@(t':ts') =
    if rank t < rank t' then t:ts else insTree (link t t') ts'

mrg :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mrg ts1           []  = ts1
mrg []            ts2 = ts2
mrg ts1@(t1:ts1') ts2@(t2:ts2')
    | rank t1 < rank t2 = t1 : mrg ts1' ts2
    | rank t1 > rank t2 = t2 : mrg ts1 ts2'
    | otherwise         = insTree (link t1 t2) (mrg ts1' ts2')

removeMinTree :: Ord a => [Tree a] -> Maybe (Tree a, [Tree a])
removeMinTree []     = Nothing
removeMinTree [t]    = return (t, [])
removeMinTree (t:ts) = do
    (t', ts') <- removeMinTree ts
    return $ if root t < root t' then (t, ts) else (t', t:ts')

instance Heap BinomialHeap where
    -- empty :: Ord a => BinomialHeap a
    empty = BH []

    -- isEmpty :: Ord a => BinomialHeap a -> Bool
    isEmpty (BH ts) = null ts

    -- insert :: Ord a => a -> BinomialHeap a -> BinomialHeap a
    insert x (BH ts) = BH (insTree (Node 0 x []) ts)

    -- merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
    merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)

    -- findMin :: Ord a => BinomialHeap a -> Maybe a
    -- findMin (BH ts) = case removeMinTree ts of
    --                       Just (t, _) -> Just (root t)
    --                       _           -> Nothing
    {- Exercise 3.5 -}
    findMin (BH []) = Nothing
    findMin (BH ts) = Just (minimum (fmap root ts))

    -- deleteMin :: Ord a => BinomialHeap a -> Maybe (BinomialHeap a)
    deleteMin (BH ts) =
        case removeMinTree ts of
            -- Just (Node _ _ ts1, ts2) -> Just (BH (mrg (reverse ts1) ts2))
            Just (t, ts') -> Just (BH (mrg (reverse (children t)) ts'))
            _             -> Nothing

-- |
-- >>> trees $ fromList [1,2]
-- [Node 1 1 [Node 0 2 []]]
--
-- >>> trees $ fromList [1,2,3,4,5,6,7,8]
-- [Node 3 1 [Node 2 5 [Node 1 7 [Node 0 8 []],Node 0 6 []],Node 1 3 [Node 0 4 []],Node 0 2 []]]
fromList :: Ord a => [a] -> BinomialHeap a
fromList = foldl' (flip insert) empty
