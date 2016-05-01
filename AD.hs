{-# LANGUAGE RankNTypes #-}
module AD () where

import Dual

dNum :: (forall a. (Num a, Eq a) => a -> a) -> (forall a. (Num a, Eq a) => a -> a)
dNum f x = du $ f (dual x 1)
