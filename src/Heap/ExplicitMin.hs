-- | Exercise 3.7
--
-- Improve the running time of findMin to O(1) by storing the minimum
-- element separately from the rest of the heap.

module Heap.ExplicitMin where

import Heap

data ExplicitMin h a = E | NE a (h a)

instance (Show (h a), Show a) => Show (ExplicitMin h a) where
    show E = "E"
    show (NE m h) = "NE " ++ show m ++ " " ++ show h

instance Heap h => Heap (ExplicitMin h) where
    -- empty :: Ord a => ExplicitMin h a
    empty = E

    -- isEmpty :: Ord a => ExplicitMin h a -> Bool
    isEmpty E = True
    isEmpty _ = False

    -- insert :: Ord a => a -> ExplicitMin h a -> ExplicitMin h a
    insert m E          = NE m (insert m empty)
    insert m (NE m' h') = NE (min m m') (insert m h')

    -- merge :: Ord a => ExplicitMin h a -> ExplicitMin h a -> ExplicitMin h a
    merge E          h          = h
    merge h          E          = h
    merge (NE m1 h1) (NE m2 h2) = NE (min m1 m2) (merge h1 h2)

    -- findMin :: Ord a => ExplicitMin h a -> Maybe a
    findMin E        = Nothing
    findMin (NE m _) = Just m

    -- deleteMin :: Ord a => ExplicitMin h a -> Maybe (ExplicitMin h a)
    deleteMin E        = Nothing
    deleteMin (NE _ h) = case deleteMin h of
                             Nothing -> Just E
                             Just h' -> do m' <- findMin h'
                                           return (NE m' h')
