{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE Trustworthy                  #-}

module Data.Hexagon.Diagonals (diagonals, diagonal, isDiagonal) where

import Control.Lens.Getter
import Control.Lens.Operators
import Control.Lens.Review
import Data.Hexagon.Types
import Data.List

diagonals :: (HexCoordinate t a, Integral a) => t a -> [ t a ]
diagonals c = fmap (view _CoordinateIso) $
              cubeDiagonals (c ^. re _CoordinateCubeIso)

diagonal :: (HexCoordinate t a, Integral a) => Direction -> t a -> t a
diagonal d = (flip (!!)) (fromEnum d) . diagonals

isDiagonal :: (Eq (t a), HexCoordinate t a, Integral a) =>
             t a -> t a -> Maybe Direction
isDiagonal a = fmap toEnum . elemIndex a . diagonals



cubeDiagonals :: Num t => CubeCoordinate t -> [CubeCoordinate t]
cubeDiagonals c =
  [ c & cubeX +~ 2 & cubeY -~ 1 & cubeZ -~ 1
  , c & cubeX +~ 1 & cubeY +~ 1 & cubeZ -~ 2
  , c & cubeX -~ 1 & cubeY +~ 2 & cubeZ -~ 1
  , c & cubeX -~ 2 & cubeY +~ 1 & cubeZ +~ 1
  , c & cubeX -~ 1 & cubeY -~ 1 & cubeZ +~ 2
  , c & cubeX +~ 1 & cubeY -~ 2 & cubeZ +~ 1
  ]
