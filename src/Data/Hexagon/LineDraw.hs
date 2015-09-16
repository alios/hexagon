{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}

module Data.Hexagon.LineDraw (lineDraw) where

import           Control.Lens
import           Data.Bits
import           Data.Hexagon.Types
import           Data.Hexagon.Distance


lineDraw :: (HexCoordinate t a, Integral a, Bits a) => t a -> t a -> [ t a ]
lineDraw a b =
  fmap (view _CoordinateIso) $ 
  lineDrawCube (a ^. re _CoordinateCubeIso) (b ^. re _CoordinateCubeIso)


lineDrawCube :: (Integral t, Bits t) =>
               CubeCoordinate t -> CubeCoordinate t -> [ CubeCoordinate t ]
lineDrawCube a b =
  let n = distance a b
  in []


cubeLerp :: Num t => CubeCoordinate t -> CubeCoordinate t -> t -> CubeCoordinate t
cubeLerp a b t =
  let x' = (a ^. cubeX + (b ^. cubeX - a ^. cubeX) * t)
      y' = (a ^. cubeY + (b ^. cubeY - a ^. cubeY) * t)
      z' = (a ^. cubeZ + (b ^. cubeZ - a ^. cubeZ) * t)
  in _CubeCoordinate # (x', y', z')
