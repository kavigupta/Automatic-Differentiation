{-# LANGUAGE RankNTypes #-}
module AD (dNum, dNum', dFrac, dFrac', dFloat, dFloat') where

import Dual

type PolyNum = forall a. (Num a, Eq a) => a -> a
type PolyFrac = forall a. (Fractional a, Eq a) => a -> a
type PolyFloat = forall a. (Floating a, Eq a) => a -> a

dNum :: PolyNum -> PolyNum
dNum f x = du $ f (dual x 1)

dNum' :: (Integral a) => a -> PolyNum -> PolyNum
dNum' 0 f = f
dNum' n f = dNum' (n-1) $ dNum f

dFrac :: PolyFrac -> PolyFrac
dFrac f x = du $ f (dual x 1)

dFrac' :: (Integral a) => a -> PolyFrac -> PolyFrac
dFrac' 0 f = f
dFrac' n f = dFrac' (n-1) $ dFrac f

dFloat :: PolyFloat -> PolyFloat
dFloat f x = du $ f (dual x 1)

dFloat' :: (Integral a) => a -> PolyFloat -> PolyFloat
dFloat' 0 f = f
dFloat' n f = dFloat' (n-1) $ dFloat f
