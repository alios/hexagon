{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Hexagon.Types
       ( -- * HexCoordinates
         HexCoordinate (..)
       , -- ** CubeCoordinates
         CubeCoordinate, CubeBase, _CubeCoordinate, cubeX, cubeY, cubeZ
       , -- ** AxialCoordinates
         AxialCoordinate, AxialBase, _AxialCoordinate, axialCol, axialRow
       , -- ** OffsetCoordinates
         OffsetCoordinate (..), OffsetBase,
         OffsetEvenQ, _OffsetEvenQ,
         OffsetEvenR, _OffsetEvenR,
         OffsetOddQ, _OffsetOddQ,
         OffsetOddR, _OffsetOddR
       , -- * Isomorphisms
         _CoordinateIso, _CubeAxialIso, _AxialOffsetIso
       , -- * Directions
         Direction(..)
       ) where

import           Control.Lens
import           Data.Bits
import           Data.Typeable (Typeable)
import qualified GHC.Generics  as GHC

type CubeBase   t = (t,t,t)
type AxialBase  t = (t,t)
type OffsetBase t = (t,t)

class (Functor t) => HexCoordinate (t :: * -> *) a where
  type CoordinateBase t a :: *
  _Coordinate :: Iso' (t a) (CoordinateBase t a)
  _CoordinateCubeIso :: (Integral a, Bits a) => Iso' (CubeCoordinate a) (t a)


class (Functor t) => OffsetCoordinate (t :: * -> *) a where
  offsetCol :: Lens' (t a) a
  offsetRow :: Lens' (t a) a
  _OffsetCoordinate :: Iso' (t a) (OffsetBase a)
  _CubeOffsetIso :: (Integral a, Bits a) => Iso' (CubeCoordinate a) (t a)

newtype CubeCoordinate t =
  CubeCoordinate (t, t, t)
  deriving (Show, Read, Eq, Ord, Bounded, Typeable, GHC.Generic)

newtype AxialCoordinate t =
  AxialCoordinate (t, t)
  deriving (Show, Read, Eq, Ord, Bounded, Typeable, GHC.Generic)

newtype OffsetEvenQ t =
  OffsetEvenQ (t, t)
  deriving (Show, Read, Eq, Ord, Bounded, Typeable, GHC.Generic)

newtype OffsetOddQ t =
  OffsetOddQ (t, t)
  deriving (Show, Read, Eq, Ord, Bounded, Typeable, GHC.Generic)

newtype OffsetEvenR t =
  OffsetEvenR (t, t)
  deriving (Show, Read, Eq, Ord, Bounded, Typeable, GHC.Generic)

newtype OffsetOddR t =
  OffsetOddR (t, t)
  deriving (Show, Read, Eq, Ord, Bounded, Typeable, GHC.Generic)

data Direction = D0 | D1 | D2 | D3 | D4 | D5
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, GHC.Generic)

_CubeCoordinate :: Iso' (CubeCoordinate t) (CoordinateBase CubeCoordinate t)
_CubeCoordinate = iso (\(CubeCoordinate a) -> a) CubeCoordinate

_AxialCoordinate :: Iso' (AxialCoordinate t) (CoordinateBase AxialCoordinate t)
_AxialCoordinate = iso (\(AxialCoordinate a) -> a) AxialCoordinate

_OffsetEvenQ :: Iso' (OffsetEvenQ t) (CoordinateBase OffsetEvenQ t)
_OffsetEvenQ = iso (\(OffsetEvenQ a) -> a) OffsetEvenQ

_OffsetOddQ :: Iso' (OffsetOddQ t) (CoordinateBase OffsetOddQ t)
_OffsetOddQ = iso (\(OffsetOddQ a) -> a) OffsetOddQ

_OffsetEvenR :: Iso' (OffsetEvenR t) (CoordinateBase OffsetEvenR t)
_OffsetEvenR = iso (\(OffsetEvenR a) -> a) OffsetEvenR

_OffsetOddR :: Iso' (OffsetOddR t) (CoordinateBase OffsetOddR t)
_OffsetOddR = iso (\(OffsetOddR a) -> a) OffsetOddR

cubeX :: Lens' (CubeCoordinate t) t
cubeX = lens (\(CubeCoordinate (x,_,_)) -> x)
        (\(CubeCoordinate (_,y,z)) x -> _CubeCoordinate # (x,y,z))

cubeY :: Lens' (CubeCoordinate t) t
cubeY = lens (\(CubeCoordinate (_,y,_)) -> y)
        (\(CubeCoordinate (x,_,z)) y -> _CubeCoordinate # (x,y,z))

cubeZ :: Lens' (CubeCoordinate t) t
cubeZ = lens (\(CubeCoordinate (_,_,z)) -> z)
        (\(CubeCoordinate (x,y,_)) z -> _CubeCoordinate # (x,y,z))


axialRow :: Lens' (AxialCoordinate t) t
axialRow = lens (\(AxialCoordinate (_,r)) -> r)
        (\(AxialCoordinate (q,_)) r -> _AxialCoordinate # (q,r))

axialCol :: Lens' (AxialCoordinate t) t
axialCol = lens (\(AxialCoordinate (q,_)) -> q)
        (\(AxialCoordinate (_,r)) q -> _AxialCoordinate # (q,r))


_CubeAxialIso :: (Num t) => Iso' (CubeCoordinate t) (AxialCoordinate t)
_CubeAxialIso =
  let a c = AxialCoordinate (c ^. cubeX, c ^. cubeZ)
      b (AxialCoordinate (q,r))    = CubeCoordinate  (q, -q-r, r)
  in iso a b


_AxialOffsetIso :: (OffsetCoordinate t a, Integral a, Bits a) =>
                  Iso' (AxialCoordinate a) (t a)
_AxialOffsetIso = from _CubeAxialIso . _CubeOffsetIso

_CoordinateIso :: (HexCoordinate s a, HexCoordinate t a, Integral a, Bits a) =>
                 Iso' (s a) (t a)
_CoordinateIso = from _CoordinateCubeIso . _CoordinateCubeIso



instance OffsetCoordinate OffsetEvenQ t where
  offsetCol = lens (\(OffsetEvenQ (c,_)) -> c)
              (\(OffsetEvenQ (_,r)) b -> (_OffsetEvenQ # (b, r)))
  offsetRow = lens (\(OffsetEvenQ (_,r)) -> r)
              (\(OffsetEvenQ (c,_)) b -> _OffsetEvenQ # (c, b))
  _OffsetCoordinate = _OffsetEvenQ
  _CubeOffsetIso =
    let a (CubeCoordinate (x,_,z)) =
          OffsetEvenQ (x, z + (x + (x .&. 1)) `div` 2)
        b (OffsetEvenQ (c, r))     =
          let x = c
              z = r - (c + (c .&. 1)) `div` 2
              y = -x-z
          in CubeCoordinate (x, y, z)
    in iso a b


instance OffsetCoordinate OffsetOddQ t where
  offsetCol = lens (\(OffsetOddQ (c,_)) -> c)
              (\(OffsetOddQ (_,r)) b -> _OffsetOddQ # (b, r))
  offsetRow = lens (\(OffsetOddQ (_,r)) -> r)
              (\(OffsetOddQ (c,_)) b -> _OffsetOddQ # (c, b))
  _OffsetCoordinate = _OffsetOddQ
  _CubeOffsetIso =
      let a (CubeCoordinate (x,_,z)) =
            OffsetOddQ (x, z + (x - (x .&. 1)) `div` 2)
          b (OffsetOddQ (c, r))     =
            let x = c
                z = r - (c - (c .&. 1)) `div` 2
                y = -x-z
            in CubeCoordinate (x, y, z)
      in iso a b

instance OffsetCoordinate OffsetEvenR t where
  offsetCol = lens (\(OffsetEvenR (c,_)) -> c)
              (\(OffsetEvenR (_,r)) b -> _OffsetEvenR # (b, r))
  offsetRow = lens (\(OffsetEvenR (_,r)) -> r)
              (\(OffsetEvenR (c,_)) b -> _OffsetEvenR # (c, b))
  _OffsetCoordinate = _OffsetEvenR
  _CubeOffsetIso =
    let a (CubeCoordinate (x,_,z)) =
          OffsetEvenR (x + (z + (z .&. 1)) `div` 2, z)
        b (OffsetEvenR (c, r))     =
          let x = c - (r + (r .&. 1)) `div` 2
              z = r
              y = -x-z
          in CubeCoordinate (x, y, z)
    in iso a b


instance OffsetCoordinate OffsetOddR t where
  offsetCol = lens (\(OffsetOddR (c,_)) -> c)
              (\(OffsetOddR (_,r)) b -> _OffsetOddR # (b, r))
  offsetRow = lens (\(OffsetOddR (_,r)) -> r)
              (\(OffsetOddR (c,_)) b -> _OffsetOddR # (c, b))
  _OffsetCoordinate = _OffsetOddR
  _CubeOffsetIso =
    let a (CubeCoordinate (x,_,z)) =
          OffsetOddR (x + (z - (z .&. 1)) `div` 2, z)
        b (OffsetOddR (c, r))     =
          let x = c - (r - (r .&. 1)) `div` 2
              z = r
              y = -x-z
          in CubeCoordinate (x, y, z)
    in iso a b


instance Functor CubeCoordinate where
  fmap f (CubeCoordinate (x,y,z)) = CubeCoordinate (f x, f y, f z)

instance Functor AxialCoordinate where
  fmap f (AxialCoordinate (q,r)) = AxialCoordinate (f q, f r)

instance Functor OffsetEvenQ where
  fmap f (OffsetEvenQ (q,r)) = OffsetEvenQ (f q, f r)

instance Functor OffsetEvenR where
  fmap f (OffsetEvenR (q,r)) = OffsetEvenR (f q, f r)

instance Functor OffsetOddQ where
  fmap f (OffsetOddQ (q,r)) = OffsetOddQ (f q, f r)

instance Functor OffsetOddR where
  fmap f (OffsetOddR (q,r)) = OffsetOddR (f q, f r)


instance HexCoordinate CubeCoordinate a where
  type CoordinateBase CubeCoordinate a = CubeBase a
  _Coordinate = _CubeCoordinate
  _CoordinateCubeIso = iso id id

instance HexCoordinate AxialCoordinate a where
  type CoordinateBase AxialCoordinate a = AxialBase a
  _Coordinate = _AxialCoordinate
  _CoordinateCubeIso = _CubeAxialIso

instance HexCoordinate OffsetEvenQ a where
  type CoordinateBase OffsetEvenQ a = OffsetBase a
  _Coordinate = _OffsetCoordinate
  _CoordinateCubeIso = _CubeOffsetIso

instance HexCoordinate OffsetEvenR a where
  type CoordinateBase OffsetEvenR a = OffsetBase a
  _Coordinate = _OffsetCoordinate
  _CoordinateCubeIso = _CubeOffsetIso

instance HexCoordinate OffsetOddQ a where
  type CoordinateBase OffsetOddQ a = OffsetBase a
  _Coordinate = _OffsetCoordinate
  _CoordinateCubeIso = _CubeOffsetIso

instance HexCoordinate OffsetOddR a where
  type CoordinateBase OffsetOddR a = OffsetBase a
  _Coordinate = _OffsetCoordinate
  _CoordinateCubeIso = _CubeOffsetIso


