module Sortable.BottomUpMergeSort (MergeSort) where

import Sortable

import Data.Foldable (foldl')

data MergeSort a = MS Int [[a]] deriving Show

mrg :: Ord a => [a] -> [a] -> [a]
mrg []         ys = ys
mrg xs         [] = xs
mrg xs@(x:xs') ys@(y:ys') = if x <= y
                            then x : mrg xs' ys
                            else y : mrg xs ys'

instance Sortable MergeSort where
    -- empty :: Ord a => MergeSort a
    empty = MS 0 []

    -- add   :: Ord a => a -> MergeSort a -> MergeSort a

    -- |
    -- >>> add 2 (add 5 (add 3 empty)) :: MergeSort Int
    -- MS 3 [[2],[3,5]]
    --
    -- >>> add 1 (add 2 (add 5 (add 3 empty))) :: MergeSort Int
    -- MS 4 [[1,2,3,5]]
    add x (MS size0 segs0) = MS (size0 + 1) (addSeg [x] segs0 size0)
      where
        addSeg seg segs size =
            if even size
            then seg : segs
            else addSeg (mrg seg (head segs)) (tail segs) (size `div` 2)

    -- sort  :: Ord a => MergeSort a -> [a]
    -- sort (MS _ segs0) = mrgAll [] segs0
    --   where
    --     mrgAll xs []         = xs
    --     mrgAll xs (seg:segs) = mrgAll (mrg xs seg) segs

    -- |
    -- >>> sort (add 2 (add 5 (add 3 empty)) :: MergeSort Int)
    -- [2,3,5]
    sort (MS _ segs) = foldl' mrg [] segs
