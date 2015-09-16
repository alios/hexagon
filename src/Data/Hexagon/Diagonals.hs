{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE Safe                  #-}

module Data.Hexagon.Diagonals (diagonals, diagonal) where

import           Control.Lens
import           Data.Bits
import           Data.Hexagon.Types


diagonals :: (HexCoordinate t a, Integral a, Bits a) => t a -> [ t a ]
diagonals c = fmap (view _CoordinateIso) $
              cubeDiagonals (c ^. re _CoordinateCubeIso)

diagonal :: (HexCoordinate t a, Integral a, Bits a) => Direction -> t a -> t a
diagonal d = (flip (!!)) (fromEnum d) . diagonals

cubeDiagonals :: Num t => CubeCoordinate t -> [CubeCoordinate t]
cubeDiagonals c =
  [ c & cubeX +~ 2 & cubeY -~ 1 & cubeZ -~ 1
  , c & cubeX +~ 1 & cubeY +~ 1 & cubeZ -~ 2
  , c & cubeX -~ 1 & cubeY +~ 2 & cubeZ -~ 1
  , c & cubeX -~ 2 & cubeY +~ 1 & cubeZ +~ 1
  , c & cubeX -~ 1 & cubeY -~ 1 & cubeZ +~ 2
  , c & cubeX +~ 1 & cubeY -~ 2 & cubeZ +~ 1
  ]
