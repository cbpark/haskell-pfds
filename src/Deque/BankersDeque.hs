module Deque.BankersDeque (BankersDeque) where

import Deque

data BankersDeque a = BD Int [a] Int [a] deriving Show

c :: Int
c = 3

check :: Int -> [a] -> Int -> [a] -> BankersDeque a
check lenf f lenr r | lenf > c * lenr + 1 = let i = (lenf + lenr) `div` 2
                                                j = lenf + lenr - i
                                                f' = take i f
                                                r' = r ++ reverse (drop i f)
                                            in BD i f' j r'
                    | lenr > c * lenf + 1 = let j = (lenf + lenr) `div` 2
                                                i = lenf + lenr - j
                                                r' = take j r
                                                f' = f ++ reverse (drop j r)
                                            in BD i f' j r'
                    | otherwise = BD lenf f lenr r

instance Deque BankersDeque where
    -- empty   :: q a
    empty = BD 0 [] 0 []

    -- isEmpty :: q a -> Bool
    isEmpty (BD lenf _ lenr _) = lenf + lenr == 0

    -- cons    :: a -> q a -> q a
    cons x (BD lenf f lenr r) = check (lenf + 1) (x:f) lenr r

    -- head    :: q a -> Maybe a
    head (BD _ []    _ _) = Nothing
    head (BD _ (x:_) _ _) = Just x

    -- tail    :: q a -> Maybe (q a)
    tail (BD _    []     _    _) = Nothing
    tail (BD lenf (_:f') lenr r) = Just (check (lenf - 1) f' lenr r)

    -- snoc    :: q a -> a -> q a
    snoc (BD lenf f lenr r) x = check lenf f (lenr + 1) (x:r)

    -- last    :: q a -> Maybe a
    last (BD _ _ _ [])    = Nothing
    last (BD _ _ _ (x:_)) = Just x

    -- init    :: q a -> Maybe (q a)
    init (BD _    _ _    [])     = Nothing
    init (BD lenf f lenr (_:r')) = Just (check lenf f (lenr - 1) r')
