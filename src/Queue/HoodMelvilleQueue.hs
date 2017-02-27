module Queue.HoodMelvilleQueue (HoodMelvilleQueue) where

import Queue

-- data ReverseState a = Working [a] [a] | Done [a] deriving Show

-- startReverse :: [a] -> ReverseState a
-- startReverse xs = Working xs []

-- exec :: ReverseState a -> ReverseState a
-- exec (Working (x:xs) xs') = Working xs (x:xs')
-- exec (Working []     xs') = Done xs'
-- exec state                = state

-- data AppendState a = Reversing [a] [a] [a]
--                    | Appending [a] [a]
--                    | Done [a]
--                    deriving Show

-- startAppend :: [a] -> [a] -> AppendState a
-- startAppend xs ys = Reversing xs [] ys

-- exec :: AppendState a -> AppendState a
-- exec (Reversing (x:xs) xs' ys) = Reversing xs (x:xs') ys
-- exec (Reversing []     xs' ys) = Appending xs' ys
-- exec (Appending [] ys)         = Done ys
-- exec state                     = state

-- data RotationState a = Reversing [a] [a] [a] [a]
--                      | Appending [a] [a]
--                      | Done [a]
--                      deriving Show

-- startRotation :: [a] -> [a] -> RotationState a
-- startRotation f r = Reversing f [] r []

-- exec :: RotationState a -> RotationState a
-- exec (Reversing (x:f) f' (y:r) r') = Reversing f (x:f') r (y:r')
-- exec (Reversing []    f' [y]   r') = Appending f' (y:r')
-- exec (Appending (x:f') r')         = Appending f' (x:r')
-- exec (Appending []     r')         = Done r'
-- exec state                         = state

data RotationState a = Idle
                     | Reversing Int [a] [a] [a] [a]
                     | Appending Int [a] [a]
                     | Done [a]
                     deriving Show

data HoodMelvilleQueue a = HM Int [a] (RotationState a) Int [a] deriving Show

-- startRotation :: [a] -> [a] -> RotationState a
-- startRotation f r = Reversing 0 f [] r []

exec :: RotationState a -> RotationState a
exec (Reversing ok (x:f) f' (y:r) r') = Reversing (ok + 1) f (x:f') r (y:r')
exec (Reversing ok []    f' [y]   r') = Appending ok f' (y:r')
exec (Appending 0  _      r')         = Done r'
exec (Appending ok (x:f') r')         = Appending (ok - 1) f' (x:r')
exec state                            = state

invalidate :: RotationState a -> RotationState a
invalidate (Reversing ok f f' r r') = Reversing (ok - 1) f f' r r'
invalidate (Appending 0  _  (_:r')) = Done r'
invalidate (Appending ok f' r')     = Appending (ok - 1) f' r'
invalidate state                    = state

exec2 :: Int -> [a] -> RotationState a -> Int -> [a] -> HoodMelvilleQueue a
exec2 lenf f state lenr r = case exec (exec state) of
                                Done newf -> HM lenf newf Idle     lenr r
                                newstate  -> HM lenf f    newstate lenr r

check :: Int -> [a] -> RotationState a -> Int -> [a] -> HoodMelvilleQueue a
check lenf f state lenr r = if lenr <= lenf
                            then exec2 lenf f state lenr r
                            else let newstate = Reversing 0 f [] r []
                                 in exec2 (lenf + lenr) f newstate 0 []

instance Queue HoodMelvilleQueue where
    -- empty   :: HoodMelvilleQueue a
    empty = HM 0 [] Idle 0 []

    -- isEmpty :: HoodMelvilleQueue a -> Bool
    isEmpty (HM lenf _ _ _ _) = lenf == 0

    -- snoc    :: HoodMelvilleQueue a -> a -> HoodMelvilleQueue a
    snoc (HM lenf f state lenr r) x = check lenf f state (lenr + 1) (x:r)

    -- head    :: HoodMelvilleQueue a -> Maybe a
    head (HM _ []    _ _ _) = Nothing
    head (HM _ (x:_) _ _ _) = Just x

    -- tail    :: HoodMelvilleQueue a -> Maybe (HoodMelvilleQueue a)
    tail (HM _    []     _     _    _) = Nothing
    tail (HM lenf (_:f') state lenr r) = Just $
        check (lenf - 1) f' (invalidate state) lenr r
