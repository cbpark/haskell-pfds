{-# LANGUAGE StrictData #-}

module Queue.BatchedQueue (BatchedQueue) where

import Queue

data BatchedQueue a = BQ [a] [a] deriving Show

check :: [a] -> [a] -> BatchedQueue a
check [] r = BQ (reverse r) []
check f  r = BQ f r

instance Queue BatchedQueue where
    -- empty   :: BatchedQueue a
    empty = BQ [] []

    -- isEmpty :: BatchedQueue a -> Bool
    isEmpty (BQ f _) = null f

    -- snoc    :: BatchedQueue a -> a -> BatchedQueue a
    snoc (BQ f r) x = check f (x:r)

    -- head    :: BatchedQueue a -> Maybe a
    head (BQ [] _)    = Nothing
    head (BQ (x:_) _) = Just x

    -- tail    :: BatchedQueue a -> Maybe (q a)
    tail (BQ [] _)    = Nothing
    tail (BQ (_:f) r) = Just (check f r)
