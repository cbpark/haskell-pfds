module CatenableList (CatenableList (..)) where

import Prelude hiding ((++))

class CatenableList c where
    empty   :: c a
    isEmpty :: c a -> Bool

    cons    :: a -> c a -> c a
    snoc    :: c a -> a -> c a
    snoc xs x = xs ++ cons x empty

    (++)    :: c a -> c a -> c a

    -- head    :: c a -> a
    head    :: c a -> Maybe a
    -- tail    :: c a -> c a
    tail    :: c a -> Maybe (c a)
