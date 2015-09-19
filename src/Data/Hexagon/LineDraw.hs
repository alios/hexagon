{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}

module Data.Hexagon.LineDraw (lineDraw) where

import           Control.Lens.Review
import           Control.Lens.Getter
import           Data.Hexagon.Distance
import           Data.Hexagon.Types
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)

lineDraw :: (HexCoordinate t a, Integral a) => t a -> t a -> Seq (t a)
lineDraw a b =
  fmap (view _CoordinateIso) $ 
  lineDrawCube (a ^. re _CoordinateCubeIso) (b ^. re _CoordinateCubeIso)


lineDrawCube :: (Integral t) =>
               CubeCoordinate t -> CubeCoordinate t -> Seq (CubeCoordinate t)
lineDrawCube a b =
  let n = (fromIntegral $ distance a b) :: Double
      a' = fmap fromIntegral a
      b' = fmap fromIntegral b      
  in fmap cubeRound . Seq.fromList $
     [cubeLerp a' b' $ 1 / n * i | i <- [0..n]]

cubeLerp :: Num t => CubeCoordinate t -> CubeCoordinate t -> t -> CubeCoordinate t
cubeLerp a b t =
  let x' = (a ^. cubeX + (b ^. cubeX - a ^. cubeX) * t)
      y' = (a ^. cubeY + (b ^. cubeY - a ^. cubeY) * t)
      z' = (a ^. cubeZ + (b ^. cubeZ - a ^. cubeZ) * t)
  in _CubeCoordinate # (x', y', z')


--cubeRound' :: (Real a, Integral b) => CubeCoordinate a -> CubeCoordinate b
--cubeRound' = cubeRound . fmap (fromRational . toRational)

cubeRound :: (Integral b) => CubeCoordinate Double -> CubeCoordinate b
cubeRound h =
  let r = fmap ((fromIntegral :: Integer -> Double) . round) h
      diff = _CubeCoordinate # ( r ^. cubeX - h ^. cubeX
                               , r ^. cubeY - h ^. cubeY
                               , r ^. cubeZ - h ^. cubeZ
                               )      
      (rx,ry,rz) =
        if ((diff ^. cubeX > diff ^. cubeY) &&
            (diff ^. cubeX > diff ^. cubeZ))
        then (-(r ^. cubeY)-(r ^. cubeZ), r ^. cubeY, r ^. cubeZ)
        else if (diff ^. cubeY > diff ^. cubeZ)
             then (r ^. cubeX, -(r ^. cubeX)-(r ^. cubeZ), r ^. cubeZ)
             else (r ^. cubeX, r ^. cubeY, -(r ^. cubeX)-(r ^. cubeY))
  in _CubeCoordinate # (round rx, round ry, round rz)

{-
function cube_round(h):
    var rx = round(h.x)
    var ry = round(h.y)
    var rz = round(h.z)

    var x_diff = abs(rx - h.x)
    var y_diff = abs(ry - h.y)
    var z_diff = abs(rz - h.z)

    if x_diff > y_diff and x_diff > z_diff:
        rx = -ry-rz
    else if y_diff > z_diff:
        ry = -rx-rz
    else:
        rz = -rx-ry

    return Cube(rx, ry, rz)
-}

