{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FiniteMap.Trie (Trie) where

import FiniteMap

import Data.Maybe (fromMaybe)
import Prelude    hiding (lookup)

data Trie mk ks a = Trie (Maybe a) (mk (Trie mk ks a))

instance FiniteMap m k => FiniteMap (Trie (m k)) [k] where
    -- empty  :: Trie (m k) [k] a
    empty = Trie Nothing empty

    -- bind   :: [k] -> a -> Trie (m k) [k] a -> Trie (m k) [k] a
    bind []     x (Trie _ m) = Trie (Just x) m
    bind (k:ks) x (Trie b m) = let t = fromMaybe empty (lookup k m)
                                   t' = bind ks x t
                               in Trie b (bind k t' m)

    -- lookup :: [k] -> Trie (m k) [k] a -> Maybe a
    lookup []     (Trie b _) = b
    lookup (k:ks) (Trie _ m) = lookup k m >>= lookup ks
