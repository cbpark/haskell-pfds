module Deque (Deque (..)) where

class Deque q where
    empty   :: q a
    isEmpty :: q a -> Bool

    cons    :: a -> q a -> q a
    -- head    :: q a -> a
    head    :: q a -> Maybe a
    -- tail    :: q a -> q a
    tail    :: q a -> Maybe (q a)

    snoc    :: q a -> a -> q a
    -- last    :: q a -> a
    last    :: q a -> Maybe a
    -- init    :: q a -> q a
    init    :: q a -> Maybe (q a)
