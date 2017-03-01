module RandomAccessList.SkewBinaryRandomAccessList (SkewList) where

import RandomAccessList

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

newtype SkewList a = SL [(Int, Tree a)] deriving Show

instance RandomAccessList SkewList where
    -- empty   :: SkewList a
    empty = SL []

    -- isEmpty :: SkewList a -> Bool
    isEmpty (SL ts) = null ts

    -- cons    :: a -> SkewList a -> SkewList a
    cons x (SL ((w1, t1):(w2, t2):ts))
        | w1 == w2 = SL ((1 + w1 + w2, Node x t1 t2):ts)
    cons x (SL ts) = SL ((1, Leaf x):ts)

    -- head    :: SkewList a -> Maybe a
    head (SL [])                  = Nothing
    head (SL ((_, Leaf x):_))     = Just x
    head (SL ((_, Node x _ _):_)) = Just x

    -- tail    :: SkewList a -> Maybe (SkewList a)
    tail (SL [])                     = Nothing
    tail (SL ((_, Leaf _):ts))       = Just (SL ts)
    tail (SL ((w, Node _ t1 t2):ts)) = Just (SL ((w', t1):(w', t2):ts))
      where w' = w `div` 2

    -- lookup  :: Int -> SkewList a -> Maybe a
    lookup i0 (SL ts0) = look i0 ts0
      where
        look _ []          = Nothing
        look i ((w, t):ts) = if i < w then lookTree w i t else look (i - w) ts

        lookTree _ i (Leaf x)       = if i == 0 then Just x else Nothing
        lookTree _ 0 (Node x _  _)  = Just x
        lookTree w i (Node _ t1 t2) = let w' = w `div` 2
                                      in if i <= w'
                                         then lookTree w' (i - 1)      t1
                                         else lookTree w' (i - 1 - w') t2


    -- update  :: Int -> a -> SkewList a -> Maybe (SkewList a)
    update i0 y (SL ts0) = SL <$> sequence (upd i0 ts0)
      where
        upd _ []          = [Nothing]
        upd i ((w, t):ts) = if i < w
                            then (case updTree w i t of
                                     Just t' -> Just (w, t')
                                     _       -> Nothing) : fmap Just ts
                            else Just (w, t) : upd (i - w) ts

        updTree _ i (Leaf _)       = if i == 0 then Just (Leaf y) else Nothing
        updTree _ 0 (Node _ t1 t2) = Just (Node y t1 t2)
        updTree w i (Node x t1 t2) = let w' = w `div` 2
                                     in if i <= w'
                                        then do t' <- updTree w' (i - 1) t1
                                                return (Node x t' t2)
                                        else do t' <- updTree w' (i - 1 - w') t2
                                                return (Node x t1 t')
