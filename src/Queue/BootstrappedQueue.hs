module Queue.BootstrappedQueue where

import Queue

import Data.Maybe (fromMaybe)
import Prelude    hiding (head, tail)

data BootstrappedQueue a = E | Q !Int ![a] (BootstrappedQueue [a]) !Int ![a]
                         deriving Show

checkQ, checkF :: Int -> [a] -> BootstrappedQueue [a] -> Int -> [a]
               -> Maybe (BootstrappedQueue a)
checkQ lenfm f m lenr r =
    if lenr <= lenfm
    then checkF lenfm          f m                    lenr r
    else checkF (lenfm + lenr) f (snoc m (reverse r)) 0    []

checkF _     [] E _    _ = Just E
checkF lenfm [] m lenr r = do h <- head m
                              t <- tail m
                              return (Q lenfm h t lenr r)
checkF lenfm f  m lenr r = Just (Q lenfm f m lenr r)

instance Queue BootstrappedQueue where
    -- empty   :: BootstrappedQueue a
    empty = Q 0 [] E 0 []

    -- isEmpty :: BootstrappedQueue a -> Bool
    isEmpty E = True
    isEmpty _ = False

    -- snoc    :: BootstrappedQueue a -> a -> BootstrappedQueue a
    snoc E                      x = Q 1 [x] E 0 []
    snoc q@(Q lenfm f m lenr r) x =
        fromMaybe q (checkQ lenfm f m (lenr + 1) (x:r))

    -- head    :: BootstrappedQueue a -> Maybe a
    head (Q _ (x:_) _ _ _) = Just x
    head _                 = Nothing

    -- tail    :: BootstrappedQueue a -> Maybe (BootstrappedQueue a)
    tail (Q lenfm (_:f') m lenr r) = checkQ (lenfm - 1) f' m lenr r
    tail _                         = Nothing
