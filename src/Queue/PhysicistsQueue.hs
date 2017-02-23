module Queue.PhysicistsQueue where

import           Queue
import qualified Stack as S

data PhysicistsQueue a = PQ [a] Int [a] Int [a] deriving Show

check :: [a] -> Int -> [a] -> Int -> [a] -> PhysicistsQueue a
check w lenf f lenr r = if lenr <= lenf
                        then checkw w lenf f lenr r
                        else checkw f (lenf + lenr) (f ++ reverse r) 0 []

checkw :: [a] -> Int -> [a] -> Int -> [a] -> PhysicistsQueue a
checkw [] lenf f = PQ f lenf f
checkw w  lenf f = PQ w lenf f

instance Queue PhysicistsQueue where
    -- empty   :: PhysicistsQueue a
    empty = PQ [] 0 [] 0 []

    -- isEmpty :: PhysicistsQueue a -> Bool
    isEmpty (PQ _ lenf _ _ _) = lenf == 0

    -- snoc    :: PhysicistsQueue a -> a -> PhysicistsQueue a
    snoc (PQ w lenf f lenr r) x = check w lenf f (lenr + 1) (x:r)

    -- head    :: PhysicistsQueue a -> Maybe a
    head (PQ []    _ _ _ _) = Nothing
    head (PQ (x:_) _ _ _ _) = Just x

    -- tail    :: PhysicistsQueue a -> Maybe (PhysicistsQueue a)
    tail (PQ []    _    _ _    _) = Nothing
    tail (PQ (_:w) lenf f lenr r) = do f' <- S.tail f
                                       return (check w (lenf - 1) f' lenr r)
