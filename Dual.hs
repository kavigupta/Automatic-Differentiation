module Dual (
        Dual, eps
    ) where

data Dual x = D x x deriving (Eq)

instance (Show x) => Show (Dual x) where
    show (D a b) = show a ++ " + " ++  show b ++ "*eps"

instance (Ord x) => Ord (Dual x) where
    compare (D a b) (D a' b')
            = case compare a b of
                EQ -> compare a' b'
                other -> other

instance (Num x, Eq x) => Num (Dual x) where
    fromInteger x = D (fromInteger x) 0
    (D a b) + (D a' b') = D (a + a') (b + b')
    (D a b) * (D a' b') = D (a * a') (a * b' + b * a')
    negate (D a b) = D (-a) (-b)
    -- For extensibility, is not entirely correct as a correct implementation
        -- requires Ord x
    abs x = x * signum x
    signum (D a b) = case signum a of
        0 -> D (signum b) 0
        x -> D x 0

eps :: (Num a) => Dual a
eps = D 0 1
