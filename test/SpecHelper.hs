module SpecHelper
       ( module Test.Hspec
       , module Test.QuickCheck
       , module Data.Hexagon
       ) where

import Test.Hspec
import Test.QuickCheck
import Data.Hexagon

import Control.Lens


instance (Num a, Arbitrary a) => Arbitrary (CubeCoordinate a) where
  arbitrary = do
    x <- arbitrary
    z <- arbitrary
    let y = -x-z
    return $ _CubeCoordinate # (x,y,z)

instance (Arbitrary a) => Arbitrary (AxialCoordinate a) where
  arbitrary = fmap (review _AxialCoordinate) arbitrary 

instance (Arbitrary a) => Arbitrary (OffsetEvenQ a) where
  arbitrary = fmap (review _OffsetEvenQ) arbitrary 

instance (Arbitrary a) => Arbitrary (OffsetEvenR a) where
  arbitrary = fmap (review _OffsetEvenR) arbitrary 

instance (Arbitrary a) => Arbitrary (OffsetOddQ a) where
  arbitrary = fmap (review _OffsetOddQ) arbitrary 

instance (Arbitrary a) => Arbitrary (OffsetOddR a) where
  arbitrary = fmap (review _OffsetOddR) arbitrary 
