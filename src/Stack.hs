module Stack where

import Data.List (tails)

class Stack s where
    empty   :: s a
    isEmpty :: s a -> Bool

    cons    :: a -> s a -> s a
    -- head    :: s a -> a
    head    :: s a -> Maybe a
    -- tail    :: s a -> s a
    tail    :: s a -> Maybe (s a)

instance Stack [] where
    -- empty :: [a]
    empty = []

    -- isEmpty :: [a] -> Bool
    isEmpty = null

    -- cons :: a -> [a] -> [a]
    cons = (:)

    -- head :: [a] -> a
    -- head :: [a] -> Maybe a
    head (x:_) = Just x
    head []    = Nothing
    -- head []    = error "empty"

    -- tail :: [a] -> [a]
    -- tail :: [a] -> Maybe [a]
    tail (_:xs) = Just xs
    tail []     = Nothing
    -- tail []     = error "empty"

data CustomStack a = Nil | Cons a (CustomStack a) deriving Show

instance Stack CustomStack where
    -- empty :: CustomStack a
    empty = Nil

    -- isEmpty :: Customstack [a] -> Bool
    isEmpty Nil = True
    isEmpty _   = False

    -- cons :: a -> CustomStack a -> CustomStack a
    cons = Cons

    -- head :: CustomStack a -> a
    -- head :: CustomStack a -> Maybe a
    head (Cons x _) = Just x
    head Nil        = Nothing
    -- head Nil        = error "empty"

    -- tail :: CustomStack a -> CustomStack a
    -- tail :: CustomStack a -> Maybe (CustomStack a)
    tail (Cons _ xs) = Just xs
    tail Nil         = Nothing
    -- tail Nil         = error "empty"

-- update :: [a] -> Int -> a -> [a]
-- update []     _ _ = error "empty"
-- update (_:xs) 0 y = y : xs
-- update (x:xs) i y = x : update xs (i - 1) y

update :: [a] -> Int -> a -> Maybe [a]
update lst n y = sequence (update' lst n)
  where
    update' (_:xs) 0 = Just y : fmap Just xs
    update' (x:xs) i = Just x : update' xs (i - 1)
    update' []     _ = [Nothing]

{- Exercise 2.1 -}
suffixes :: [a] -> [[a]]
-- suffixes lst = let tailGo xs = xs : case xs of
--                                         []      -> []
--                                         (_:xs') -> tailGo xs'
--                in tailGo lst
suffixes = tails
