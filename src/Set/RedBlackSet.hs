{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Set.RedBlackSet (RedBlackSet) where

import Set

data Color = R | B deriving Show

data RedBlackSet a = E | T Color !(RedBlackSet a) a !(RedBlackSet a)
                   deriving Show

-- balance :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
-- balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
-- balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
-- balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
-- balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
-- balance color a x b                 = T color a x b

-- balanceL :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
-- balanceL B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
-- balanceL B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
-- balanceL color a x b                 = T color a x b

-- balanceR :: Color -> RedBlackSet a -> a -> RedBlackSet a -> RedBlackSet a
-- balanceR B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
-- balanceR B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
-- balanceR color a x b                 = T color a x b

balanceLL, balanceLR, balanceRL, balanceRR :: Color
                                           -> RedBlackSet a
                                           -> a
                                           -> RedBlackSet a
                                           -> RedBlackSet a
balanceLL B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balanceLL color a x b                 = T color a x b

balanceLR B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balanceLR color a x b                 = T color a x b

balanceRL B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balanceRL color a x b                 = T color a x b

balanceRR B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balanceRR color a x b                 = T color a x b

instance Ord a => Set RedBlackSet a where
    -- empty :: RedBlackSet a
    empty = E

    -- insert :: a -> RedBlackSet a -> RedBlackSet a
    -- insert x s0 = let T _ a y b = ins s0 in T B a y b
    --   where
    --     ins E = T R E x E
    --     ins s@(T color a y b) | x < y     = balance color (ins a) y b
    --                           | x > y     = balance color a y (ins b)
    --                           | otherwise = s
    {- Exercise 3.10(a) -}
    -- insert x s0 = let T _ a y b = ins s0 in T B a y b
    --   where
    --     ins E = T R E x E
    --     ins s@(T color a y b) | x < y     = balanceL color (ins a) y b
    --                           | x > y     = balanceR color a y (ins b)
    --                           | otherwise = s
    {- Exercise 3.10(b) -}
    insert x s0 = let T _ a y b = ins s0 in T B a y b
      where
        ins E = T R E x E
        ins s@(T color a y b)
            | x < y = case a of
                          E         -> T color (ins a) y b
                          T _ _ z _ -> if x <= z
                                       then balanceLL color (ins a) y b
                                       else balanceLR color (ins a) y b
            | x > y = case b of
                          E         -> T color a y (ins b)
                          T _ _ z _ -> if x <= z
                                       then balanceRL color a y (ins b)
                                       else balanceRR color a y (ins b)
            | otherwise = s

    -- member :: a -> RedBlackSet a -> Bool
    member _ E = False
    member x (T _ a y b) | x < y     = member x a
                         | x > y     = member x b
                         | otherwise = True
