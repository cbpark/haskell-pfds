{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Set.UnbalancedSet where

import Set
import FiniteMap

import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

data UnbalancedSet a = E | T !(UnbalancedSet a) a !(UnbalancedSet a)
                     deriving Show

instance Ord a => Set UnbalancedSet a where
    -- empty :: UnbalancedSet a
    empty = E

    -- insert :: a -> UnbalancedSet a -> UnbalancedSet a
    -- insert x E = T E x E
    -- insert x s@(T a y b) | x < y     = T (insert x a) y b
    --                      | x > y     = T a y (insert x b)
    --                      | otherwise = s

    {- Exercise 2.3 -}
    -- insert x s = fromMaybe s (insert' s)
    --   where
    --     insert' E = Just (T E x E)
    --     insert' (T a y b) | x < y     = do l <- insert' a
    --                                        return (T l y b)
    --                       | x > y     = do r <- insert' b
    --                                        return (T a y r)
    --                       | otherwise = Nothing

    {- Exercise 2.4 -}
    insert x s = fromMaybe s (insert' Nothing s)
        where
          insert' Nothing  E = Just (T E x E)
          insert' (Just y) E = if x == y then Nothing else Just (T E x E)
          insert' e        (T a y b) = if x < y
                                       then do l <- insert' e a
                                               return (T l y b)
                                       else do r <- insert' (Just y) b
                                               return (T a y r)

    -- member :: a -> UnbalancedSet a -> Bool
    -- member _ E = False
    -- member x (T a y b) | x < y     = member x a
    --                    | x > y     = member x b
    --                    | otherwise = True

    {- Exercise 2.2 -}
    member x = member' Nothing
      where
        member' Nothing  E         = False
        member' (Just y) E         = x == y
        member' e        (T a y b) = if x < y
                                     then member' e a
                                     else member' (Just y) b

{- Exercise 2.5(a) -}
complete :: a -> Int -> UnbalancedSet a
complete x d = if d <= 0
               then E
               else let s = complete x (d - 1) in T s x s

{- Exercise 2.5(b) -}
balanced :: a -> Int -> UnbalancedSet a
balanced x = fst . create2
  where
    create2 m | m <= 0    = (E, T E x E)
              | odd m     = let (s0, s1) = create2 ((m - 1) `div` 2)
                            in (T s0 x s0, T s1 x s0)
              | otherwise = let (s0, s1) = create2 (m `div` 2 - 1)
                            in (T s1 x s0, T s1 x s1)

{- Exercise 2.6 -}
newtype UnbalancedMap k a = UM { getMap :: UnbalancedSet (k, a) }

instance Ord k => FiniteMap UnbalancedMap k where
    -- empty :: UnbalancedMap k a
    empty = UM E

    -- bind :: k -> a -> UnbalancedMap k a -> UnbalancedMap k a
    bind k v m@(UM (T a (k', v') b))
        | k < k'    = UM (T (getMap (bind k v (UM a))) (k', v') b)
        | k > k'    = UM (T a (k', v') (getMap (bind k v (UM b))))
        | otherwise = m
    bind k v _ = UM (T E (k, v) E)

    -- lookup :: k -> UnbalancedMap k a -> Maybe a
    lookup k0 (UM (T a (k, v) b)) | k0 < k    = lookup k0 (UM a)
                                  | k0 > k    = lookup k0 (UM b)
                                  | otherwise = Just v
    lookup _  _ = Nothing
