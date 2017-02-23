module Queue.BankersQueue where

import Queue

data BankersQueue a = BQ Int [a] Int [a] deriving Show

check :: Int -> [a] -> Int -> [a] -> BankersQueue a
check lenf f lenr r = if lenr <= lenf
                      then BQ lenf f lenr r
                      else BQ (lenf + lenr) (f ++ reverse r) 0 []

instance Queue BankersQueue where
    -- empty   :: BankersQueue a
    empty = BQ 0 [] 0 []

    -- isEmpty :: BankersQueue a -> Bool
    isEmpty (BQ lenf _ _ _) = lenf == 0

    -- snoc    :: BankersQueue a -> a -> BankersQueue a
    snoc (BQ lenf f lenr r) x = check lenf f (lenr + 1) (x:r)

    -- head    :: BankersQueue a -> Maybe a
    head (BQ _ []    _ _) = Nothing
    head (BQ _ (x:_) _ _) = Just x

    -- tail    :: BankersQueue a -> Maybe (BankersQueue a)
    tail (BQ _    []     _    _) = Nothing
    tail (BQ lenf (_:f') lenr r) = Just (check (lenf - 1) f' lenr r)
