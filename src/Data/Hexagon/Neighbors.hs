{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy           #-}

module Data.Hexagon.Neighbors (HasNeighbors (..), neighbor, isNeighbor) where

import           Control.Lens.Operators
import           Data.Hexagon.Types
import           Data.List

import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (fromJust)
-- | a 'HexCoordinate' which has neighbors.
class (Integral a) => HasNeighbors (t :: * -> *) a where
  -- | get all 6 neighbors of a 'HexCoordinate'
  directions :: t a -> Map Direction (t a)


-- | given a 'Direction' return that neighbor
neighbor :: (HasNeighbors t a) => Direction -> t a -> t a
neighbor d = fromJust . Map.lookup d . directions

-- | in case of a being a neighbor of b return 'Just' 'Direction' else 'Nothing'
isNeighbor :: (HasNeighbors t a, Eq (t a)) =>  t a -> t a -> Maybe Direction
isNeighbor a = findMapKeyByValue a . directions


findMapKeyByValue :: (Eq a) => a -> Map k a -> Maybe k
findMapKeyByValue a = fmap fst . find ((==) a . snd) . Map.toList


cubeDirections :: Num t => CubeCoordinate t -> Map Direction (CubeCoordinate t)
cubeDirections c = Map.fromList
  [ (D0, c & cubeX +~ 1 & cubeY -~ 1)
  , (D1, c & cubeX +~ 1 & cubeZ -~ 1)
  , (D2, c & cubeY +~ 1 & cubeZ -~ 1)
  , (D3, c & cubeX -~ 1 & cubeY +~ 1)
  , (D4, c & cubeX -~ 1 & cubeZ +~ 1)
  , (D5, c & cubeY -~ 1 & cubeZ +~ 1)
  ]

axialDirections :: Num t => AxialCoordinate t -> Map Direction (AxialCoordinate t)
axialDirections c = Map.fromList
  [ (D0, c & axialCol +~ 1)
  , (D1, c & axialCol +~ 1 & axialRow -~ 1)
  , (D2, c & axialRow -~ 1)
  , (D3, c & axialCol -~ 1)
  , (D4, c & axialCol -~ 1 & axialRow +~ 1)
  , (D5, c & axialRow +~ 1)
  ]


offsetEvenQDirections :: (Integral t) =>
                        OffsetEvenQ t -> Map Direction (OffsetEvenQ t)
offsetEvenQDirections c = Map.fromList $
  if (even $ c ^. offsetCol)
  then [ (D0, c & offsetCol +~ 1 & offsetRow +~ 1)
       , (D1, c & offsetCol +~ 1)
       , (D2, c & offsetRow -~ 1)
       , (D3, c & offsetCol -~ 1)
       , (D4, c & offsetCol -~ 1 & offsetRow +~ 1)
       , (D5, c & offsetRow +~ 1)
       ]
  else [ (D0, c & offsetCol +~ 1)
       , (D1, c & offsetCol +~ 1 & offsetRow -~ 1)
       , (D2, c & offsetRow -~ 1)
       , (D3, c & offsetCol -~ 1 & offsetRow -~ 1)
       , (D4, c & offsetCol -~ 1)
       , (D5, c & offsetRow +~ 1)
       ]


offsetOddQDirections :: (Integral t) => OffsetOddQ t -> Map Direction (OffsetOddQ t)
offsetOddQDirections c = Map.fromList $
  if (even $ c ^. offsetCol)
  then [ (D0, c & offsetCol +~ 1)
       , (D1, c & offsetCol +~ 1 & offsetRow -~ 1)
       , (D2, c & offsetRow -~ 1)
       , (D3, c & offsetCol -~ 1 & offsetRow -~ 1)
       , (D4, c & offsetCol -~ 1)
       , (D5, c & offsetRow +~ 1)
   ]
  else [ (D0, c & offsetCol +~ 1 & offsetRow +~ 1)
       , (D1, c & offsetCol +~ 1)
       , (D2, c & offsetRow -~ 1)
       , (D3, c & offsetCol -~ 1)
       , (D4, c & offsetCol -~ 1 & offsetRow +~ 1)
       , (D5, c & offsetRow +~ 1)
       ]

offsetEvenRDirections :: (Integral t) => OffsetEvenR t -> Map Direction (OffsetEvenR t)
offsetEvenRDirections c = Map.fromList $
  if (even $ c ^. offsetRow)
  then [ (D0, c & offsetCol +~ 1)
       , (D1, c & offsetCol +~ 1 & offsetRow -~ 1)
       , (D2, c &                  offsetRow -~ 1)
       , (D3, c & offsetCol -~ 1)
       , (D4, c &                  offsetRow +~ 1)
       , (D5, c & offsetCol +~ 1 & offsetRow +~ 1)
       ]
  else [ (D0, c & offsetCol +~ 1)
       , (D1, c &                  offsetRow -~ 1)
       , (D2, c & offsetCol -~ 1 & offsetRow -~ 1)
       , (D3, c & offsetCol -~ 1)
       , (D4, c & offsetCol -~ 1 & offsetRow +~ 1)
       , (D5, c &                  offsetRow +~ 1)
       ]


offsetOddRDirections :: (Integral t) => OffsetOddR t -> Map Direction (OffsetOddR t)
offsetOddRDirections c = Map.fromList $
  if (even $ c ^. offsetRow)
  then [ (D0, c & offsetCol +~ 1)
       , (D1, c & offsetRow -~ 1)
       , (D2, c & offsetCol -~ 1 & offsetRow -~ 1)
       , (D3, c & offsetCol -~ 1)
       , (D4, c & offsetCol -~ 1 & offsetRow +~ 1)
       , (D5, c & offsetRow +~ 1)
       ]
  else [ (D0, c & offsetCol +~ 1)
       , (D1, c & offsetCol +~ 1 & offsetRow -~ 1)
       , (D2, c & offsetRow -~ 1)
       , (D3, c & offsetCol -~ 1)
       , (D4, c & offsetRow +~ 1)
       , (D5, c & offsetCol +~ 1 & offsetRow +~ 1)
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
