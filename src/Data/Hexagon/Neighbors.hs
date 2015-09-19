{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}

module Data.Hexagon.Neighbors (HasNeighbors (..)) where

import           Control.Lens.Operators
import           Data.Hexagon.Types


-- | a 'HexCoordinate' which has neighbors.
class (Integral a) => HasNeighbors (t :: * -> *) a where
  -- | get all 6 neighbors of a 'HexCoordinate'
  directions :: t a -> [ t a ]
  -- | given a 'Direction' return that neighbor
  neighbor :: Direction -> t a -> t a
  neighbor d = (flip (!!)) (fromEnum d) . directions

cubeDirections :: Num t => CubeCoordinate t -> [ CubeCoordinate t ]
cubeDirections c =
  [ c & cubeX +~ 1 & cubeY -~ 1
  , c & cubeX +~ 1 & cubeZ -~ 1
  , c & cubeY +~ 1 & cubeZ -~ 1
  , c & cubeX -~ 1 & cubeY +~ 1
  , c & cubeX -~ 1 & cubeZ +~ 1
  , c & cubeY -~ 1 & cubeZ +~ 1
  ]

axialDirections :: Num t => AxialCoordinate t -> [ AxialCoordinate t ]
axialDirections c =
  [ c & axialCol +~ 1
  , c & axialCol +~ 1 & axialRow -~ 1
  , c & axialRow -~ 1
  , c & axialCol -~ 1
  , c & axialCol -~ 1 & axialRow +~ 1
  , c & axialRow +~ 1
  ]


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
       , c &                  offsetRow -~ 1
       , c & offsetCol -~ 1
       , c &                  offsetRow +~ 1
       , c & offsetCol +~ 1 & offsetRow +~ 1
       ]
  else [ c & offsetCol +~ 1
       , c &                  offsetRow -~ 1
       , c & offsetCol -~ 1 & offsetRow -~ 1
       , c & offsetCol -~ 1
       , c & offsetCol -~ 1 & offsetRow +~ 1
       , c &                  offsetRow +~ 1
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



instance (Integral a) => HasNeighbors CubeCoordinate a where
  directions = cubeDirections

instance (Integral a) => HasNeighbors AxialCoordinate a where
  directions = axialDirections

instance (Integral a) => HasNeighbors OffsetEvenQ a where
  directions = offsetEvenQDirections

instance (Integral a) => HasNeighbors OffsetOddQ a where
  directions = offsetOddQDirections

instance (Integral a) => HasNeighbors OffsetEvenR a where
  directions = offsetEvenRDirections

instance (Integral a) => HasNeighbors OffsetOddR a where
  directions = offsetOddRDirections
