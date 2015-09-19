{-# LANGUAGE Safe #-}

module Data.Hexagon
       ( -- * Data Types
         HexCoordinate (..), CubeCoordinate, AxialCoordinate,
         OffsetEvenQ, OffsetEvenR, OffsetOddQ, OffsetOddR,
         Direction (..)
       , -- * Algorithms
         -- ** Neighbors
         HasNeighbors (..), neighbor, isNeighbor
       , -- ** Diagonals
         diagonals, diagonal, isDiagonal
       , -- ** Distance
         distance
       , -- ** Line Draw
         lineDraw
       , -- ** Range
         range
       ) where


import           Data.Hexagon.Diagonals
import           Data.Hexagon.Distance
import           Data.Hexagon.LineDraw
import           Data.Hexagon.Neighbors
import           Data.Hexagon.Range
import           Data.Hexagon.Types
