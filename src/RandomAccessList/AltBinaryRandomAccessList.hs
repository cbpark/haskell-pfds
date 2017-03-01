module RandomAccessList.AltBinaryRandomAccessList (BinaryList) where

import RandomAccessList

import Prelude hiding (lookup)

data BinaryList a = Nil | Zero (BinaryList (a, a)) | One a (BinaryList (a, a))
                  deriving Show

uncons :: BinaryList a -> Maybe (a, BinaryList a)
uncons Nil         = Nothing
uncons (One x Nil) = Just (x, Nil)
uncons (One x ps)  = Just (x, Zero ps)
uncons (Zero ps)   = uncons ps >>= (\((x, y), ps') -> return (x, One y ps'))

fupdate :: (a -> a) -> Int -> BinaryList a -> Maybe (BinaryList a)
fupdate _ _ Nil        = Nothing
fupdate f 0 (One x ps) = Just (One (f x) ps)
fupdate f i (One x ps) = fmap (cons x) (fupdate f (i - 1) (Zero ps))
fupdate f i (Zero ps)  = Zero <$> fupdate f' (i `div` 2) ps
  where f' (x, y) = if even i then (f x, y) else (x, f y)

instance RandomAccessList BinaryList where
    -- empty   :: BinaryList a
    empty = Nil

    -- isEmpty :: BinaryList a -> Bool
    isEmpty Nil = True
    isEmpty _   = False

    -- cons    :: a -> BinaryList a -> BinaryList a
    cons x Nil        = One x Nil
    cons x (Zero ps)  = One x ps
    cons x (One y ps) = Zero (cons (x, y) ps)

    -- head    :: BinaryList a -> Maybe a
    head Nil = Nothing
    head xs  = fmap fst (uncons xs)

    -- tail    :: BinaryList a -> Maybe (BinaryList a)
    tail Nil = Nothing
    tail xs  = fmap snd (uncons xs)

    -- lookup  :: Int -> BinaryList a -> Maybe a
    lookup _ Nil        = Nothing
    lookup i (One x ps) = if i == 0 then Just x else lookup (i - 1) (Zero ps)
    lookup i (Zero ps)  = do (x, y) <- lookup (i `div` 2) ps
                             return (if even i then x else y)

    -- update  :: Int -> a -> BinaryList a -> Maybe (r a)
    update i y = fupdate (const y) i
