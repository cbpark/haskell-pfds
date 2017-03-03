{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FiniteMap.TrieOfTrees (Tree (..), Trie) where

import FiniteMap

import Data.Maybe (fromMaybe)
import Prelude    hiding (lookup)

data Tree a = E | T a (Tree a) (Tree a)

data Trie mk ks a = Trie (Maybe a) (mk (Trie mk ks (Trie mk ks a)))

instance FiniteMap m k => FiniteMap (Trie (m k)) (Tree k) where
    -- empty  :: Trie (m k) (Tree k) a
    empty = Trie Nothing empty

    -- bind   :: Tree k -> a -> Trie (m k) (Tree k) a -> Trie (m k) (Tree k) a
    bind E x         (Trie _ m) = Trie (Just x) m
    bind (T k a b) x (Trie v m) = let tt = fromMaybe empty (lookup k m)
                                      t  = fromMaybe empty (lookup a tt)
                                      t'  = bind b x t
                                      tt' = bind a t' tt
                                  in Trie v (bind k tt' m)

    -- lookup :: Tree k -> Trie (m k) (Tree k) a -> Maybe a
    lookup E         (Trie v _) = v
    lookup (T k a b) (Trie _ m) = do m'  <- lookup k m
                                     m'' <- lookup a m'
                                     lookup b m''
