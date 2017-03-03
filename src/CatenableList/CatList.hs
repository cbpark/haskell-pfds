module CatenableList.CatList (CatList) where

import           CatenableList
import           Queue         (Queue)
import qualified Queue         as Q

import           Data.Maybe    (fromJust)
import           Prelude       hiding ((++))

data CatList q a = E | C a (q (CatList q a))

link :: Queue q => CatList q a -> CatList q a -> Maybe (CatList q a)
link (C x q) s = Just (C x (Q.snoc q s))
link E       _ = Nothing

instance Queue q => CatenableList (CatList q) where
    -- empty   :: CatList q a
    empty = E

    -- isEmpty :: CatList q a -> Bool
    isEmpty E = True
    isEmpty _ = False

    -- cons    :: a -> CatList q a -> CatList q a
    cons x xs = C x Q.empty ++ xs

    -- snoc    :: CatList q a -> a -> CatList q a
    snoc xs x = xs ++ C x Q.empty

    -- (++)    :: CatList q a -> CatList q a -> CatList q a
    xs ++ E  = xs
    E  ++ ys = ys
    xs ++ ys = fromJust (link xs ys)

    -- head    :: CatList q a -> Maybe a
    head E       = Nothing
    head (C x _) = Just x

    -- tail    :: CatList q a -> Maybe (CatList q a)
    tail E        = Nothing
    tail (C _ q0) = if Q.isEmpty q0 then Just E else linkAll q0
      where
        linkAll q = do t  <- Q.head q
                       q' <- Q.tail q
                       q'' <- linkAll q'
                       t'  <- link t q''
                       return (if Q.isEmpty q' then t else t')
