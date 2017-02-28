module RandomAccessList (RandomAccessList (..)) where

class RandomAccessList r where
    empty   :: r a
    isEmpty :: r a -> Bool

    cons    :: a -> r a -> r a
    -- head    :: r a -> a
    head    :: r a -> Maybe a
    -- tail    :: r a -> r a
    tail    :: r a -> Maybe (r a)

    -- lookup  :: Int -> r a -> a
    lookup  :: Int -> r a -> Maybe a
    -- update  :: Int -> a -> r a -> r a
    update  :: Int -> a -> r a -> Maybe (r a)
