{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}

module Data.Hexagon.Distance (distance) where

import           Control.Lens
import           Data.Bits
import           Data.Hexagon.Types


distance :: (HexCoordinate t a, Integral a, Bits a) => t a -> t a -> a
distance a b =
  cubeDistance (a ^. re _CoordinateCubeIso) (b ^. re _CoordinateCubeIso)

cubeDistance :: (Integral a, Bits a) => CubeCoordinate a -> CubeCoordinate a -> a
cubeDistance a b =
  let dx = abs $ a ^. cubeX - b ^. cubeX
      dy = abs $ a ^. cubeY - b ^. cubeY
      dz = abs $ a ^. cubeZ - b ^. cubeZ
  in max dx $ max dy dz

