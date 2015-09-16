{-# LANGUAGE Safe #-}

module Data.Hexagon
       ( -- * Data Types
         module Data.Hexagon.Types
       ) where

import           Control.Lens
import           Data.Hexagon.Types


cubeDirections :: Num t => CubeCoordinate t -> [ CubeCoordinate t ]
cubeDirections c =
  [ c & cubeX +~ 1 & cubeY -~ 1
  , c & cubeX +~ 1 & cubeZ -~ 1
  , c & cubeY +~ 1 & cubeZ -~ 1
  , c & cubeX -~ 1 & cubeY +~ 1
  , c & cubeX -~ 1 & cubeZ +~ 1
  , c & cubeY -~ 1 & cubeZ +~ 1
  ]

cubeNeighbour :: (Num t) => Direction -> CubeCoordinate t -> CubeCoordinate t
cubeNeighbour d = (flip (!!)) (fromEnum d) . cubeDirections
