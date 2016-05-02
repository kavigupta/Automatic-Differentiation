module Dual (
        Dual, eps, re, du, dual
    ) where

data Dual x = D x x deriving (Eq)

dual :: x -> x -> Dual x
dual = D

re :: Dual x -> x
re (D a _) = a

du :: Dual x -> x
du (D _ b) = b

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

instance (Eq x, Fractional x) => Fractional (Dual x) where
    recip (D a b) = D (1 / a) (-b / (a * a))
    fromRational x = D (fromRational x) 0

lift :: (Num a) => (a -> a) -> (a -> a) -> (Dual a -> Dual a)
lift f f' (D a b) = D (f a) (b * f' a)

instance (Eq x, Floating x) => Floating (Dual x) where
    pi = D pi 0
    exp = lift exp exp
    log = lift log (abs . (1/))
    sqrt = lift sqrt (\x -> 1 / (2 * sqrt x))
    a ** b = exp $ b * log a
    logBase a b = log b / log a
    sin = lift sin cos
    cos = lift cos (negate . sin)
    tan = lift tan (\x -> 1 / (cos x * cos x))
    asin = lift asin (\x -> 1 / sqrt (1 - x * x))
    acos = lift acos (\x -> -1 / sqrt (1 - x * x))
    atan = lift atan (\x -> 1 / (1 + x * x))
    sinh = lift sinh cosh
    cosh = lift cosh sinh
    tanh = lift tanh (\x -> 1 - tanh x * tanh x)
    asinh = lift asinh (\x -> 1 / sqrt (x * x + 1))
    acosh = lift acosh (\x -> 1 / sqrt (x * x - 1))
    atanh = lift atanh (\x -> 1 / (1 - x * x))

eps :: (Num a) => Dual a
eps = D 0 1
