{-# LANGUAGE Safe #-}

module Data.Hexagon.Neighbors
       ( -- * CubeCoordinate
         cubeDirections, cubeNeighbor
       , -- * AxialCoordinate
         axialDirections, axialNeighbor
       , -- * OffsetCoordinate
         offsetEvenQDirections,
         offsetOddQDirections,
         offsetEvenRDirections,
         offsetOddRDirections
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

cubeNeighbor :: (Num t) => Direction -> CubeCoordinate t -> CubeCoordinate t
cubeNeighbor d = (flip (!!)) (fromEnum d) . cubeDirections

axialDirections :: Num t => AxialCoordinate t -> [ AxialCoordinate t ]
axialDirections c =
  [ c & axialCol +~ 1
  , c & axialCol +~ 1 & axialRow -~ 1
  , c & axialRow -~ 1
  , c & axialCol -~ 1
  , c & axialCol -~ 1 & axialRow +~ 1
  , c & axialRow +~ 1
  ]

axialNeighbor :: (Num t) => Direction -> AxialCoordinate t -> AxialCoordinate t
axialNeighbor d = (flip (!!)) (fromEnum d) . axialDirections

offsetEvenQDirections :: (Integral t) => OffsetEvenQ t -> [OffsetEvenQ t]
offsetEvenQDirections c =
  if (even $ c ^. offsetCol)
  then [ c & offsetCol +~ 1 & offsetRow +~ 1
       , c & offsetCol +~ 1
       , c & offsetRow -~ 1
       , c & offsetCol -~ 1
       , c & offsetCol -~ 1 & offsetRow +~ 1
       , c & offsetRow +~ 1
       ]
  else [ c & offsetCol +~ 1
       , c & offsetCol +~ 1 & offsetRow -~ 1
       , c & offsetRow -~ 1
       , c & offsetCol -~ 1 & offsetRow -~ 1
       , c & offsetCol -~ 1
       , c & offsetRow +~ 1
       ]


offsetOddQDirections :: (Integral t) => OffsetOddQ t -> [OffsetOddQ t]
offsetOddQDirections c =
  if (even $ c ^. offsetCol)
  then [ c & offsetCol +~ 1
       , c & offsetCol +~ 1 & offsetRow -~ 1
       , c & offsetRow -~ 1
       , c & offsetCol -~ 1 & offsetRow -~ 1
       , c & offsetCol -~ 1
       , c & offsetRow +~ 1
   ]
  else [ c & offsetCol +~ 1 & offsetRow +~ 1
       , c & offsetCol +~ 1
       , c & offsetRow -~ 1
       , c & offsetCol -~ 1
       , c & offsetCol -~ 1 & offsetRow +~ 1
       , c & offsetRow +~ 1
       ]




offsetEvenRDirections :: (Integral t) => OffsetEvenR t -> [OffsetEvenR t]
offsetEvenRDirections c =
  if (even $ c ^. offsetRow)
  then [ c & offsetCol +~ 1
       , c & offsetCol +~ 1 & offsetRow -~ 1
       , c & offsetRow -~ 1
       , c & offsetCol -~ 1
       , c & offsetRow +~ 1
       , c & offsetCol +~ 1 & offsetRow +~ 1
       ]
  else [ c & offsetCol +~ 1 & offsetRow +~ 1
       , c & offsetCol +~ 1 & offsetRow +~ 1
       , c & offsetCol +~ 1 & offsetRow +~ 1
       , c & offsetCol +~ 1 & offsetRow +~ 1
       , c & offsetCol +~ 1 & offsetRow +~ 1
       , c & offsetCol +~ 1 & offsetRow +~ 1
       ]


offsetOddRDirections :: (Integral t) => OffsetOddR t -> [OffsetOddR t]
offsetOddRDirections c =
  if (even $ c ^. offsetRow)
  then [ c & offsetCol +~ 1
       , c & offsetRow -~ 1
       , c & offsetCol -~ 1 & offsetRow -~ 1
       , c & offsetCol -~ 1
       , c & offsetCol -~ 1 & offsetRow +~ 1
       , c & offsetRow +~ 1
       ]
  else [ c & offsetCol +~ 1
       , c & offsetCol +~ 1 & offsetRow -~ 1
       , c & offsetRow -~ 1
       , c & offsetCol -~ 1
       , c & offsetRow +~ 1
       , c & offsetCol +~ 1 & offsetRow +~ 1
       ]



