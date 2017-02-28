module RandomAccessList.BinaryRandomAcessList (BinaryList) where

import RandomAccessList

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show

data Digit a = Zero | One (Tree a) deriving Show

newtype BinaryList a = BL [Digit a] deriving Show

size :: Tree a -> Int
size (Leaf _)     = 1
size (Node w _ _) = w

link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

consTree :: Tree a -> [Digit a] -> [Digit a]
consTree t  []          = [One t]
consTree t  (Zero:ts)   = One t : ts
consTree t1 (One t2:ts) = Zero : consTree (link t1 t2) ts

unconsTree :: [Digit a] -> Maybe (Tree a, [Digit a])
unconsTree []         = Nothing
unconsTree [One t]    = Just (t, [])
unconsTree (One t:ts) = Just (t, Zero:ts)
unconsTree (Zero:ts)  = do (Node _ t1 t2, ts') <- unconsTree ts
                           return (t1, One t2:ts')

instance RandomAccessList BinaryList where
    -- empty   :: BinaryList a
    empty = BL []

    -- isEmpty :: BinaryList a -> Bool
    isEmpty (BL ts) = null ts

    -- cons    :: a -> BinaryList a -> BinaryList a
    cons x (BL ts) = BL (consTree (Leaf x) ts)

    -- head    :: BinaryList a -> Maybe a
    head (BL ts) = unconsTree ts >>= (\(Leaf x, _) -> return x)

    -- tail    :: BinaryList a -> Maybe (BinaryList a)
    tail (BL ts) = unconsTree ts >>= (\(_, ts') -> return (BL ts'))

    -- lookup  :: Int -> BinaryList a -> Maybe a
    lookup i0 (BL ts0) = look i0 ts0
      where
        look _ []         = Nothing
        look i (Zero:ts)  = look i ts
        look i (One t:ts) =
            if i < size t then lookTree i t else look (i - size t) ts

        lookTree i (Leaf x) = if i == 0 then Just x else Nothing
        lookTree i (Node w t1 t2) =
            if i < w `div` 2 then lookTree i t1 else lookTree (i - w `div` 2) t2

    -- update  :: Int -> a -> BinaryList a -> Maybe (BinaryList a)
    update i0 y (BL ts0) = BL <$> sequence (upd i0 ts0)
      where
        upd _ []         = [Nothing]
        upd i (Zero:ts)  = Just Zero : upd i ts
        upd i (One t:ts) = if i < size t
                           then fmap One (updTree i t) : fmap Just ts
                           else Just (One t) : upd (i - size t) ts

        updTree i (Leaf _)       = if i == 0 then Just (Leaf y) else Nothing
        updTree i (Node w t1 t2) = if i < w `div` 2
                                   then do t' <- updTree i t1
                                           return (Node w t' t2)
                                   else do t' <- updTree (i - w `div` 2) t2
                                           return (Node w t1 t')
