{-# LANGUAGE MultiParamTypeClasses #-}

module Set (Set (..)) where

import Data.Foldable (foldl')

class Set s a where
    empty  :: s a
    insert :: a -> s a -> s a
    member :: a -> s a -> Bool

    fromList :: [a] -> s a
    fromList = foldl' (flip insert) empty
