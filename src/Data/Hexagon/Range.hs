{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe              #-}

module Data.Hexagon.Range (range) where

import Control.Lens
import Data.Bits
import Data.Hexagon.Types

range :: (HexCoordinate t a, Integral a, Bits a) => a -> t a -> [t a]
range n c = fmap (view _CoordinateCubeIso) . cubeRange n $
            c ^. re _CoordinateCubeIso

cubeRange :: (Integral a) => a -> CubeCoordinate a -> [CubeCoordinate a]
cubeRange n c =
  let dxys = [(dx, [(max (-n) (-dx-n)) .. (min (n) (n-dx))] ) | dx <- [-n .. n]]
      addDelta dx dy = c & cubeX +~ dx & cubeY +~ dy & cubeZ +~ (-dx-dy)
      applyDelta (dx, dys) = fmap (addDelta dx) dys
  in mconcat . fmap applyDelta $ dxys

