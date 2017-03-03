module CatenableDeque (CatenableDeque (..)) where

import Deque (Deque)

class Deque d => CatenableDeque d where
    (++) :: d a -> d a -> d a
