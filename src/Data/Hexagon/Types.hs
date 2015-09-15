{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
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
  _Coordinate :: Iso' (CoordinateBase t a) (t a)
  _CoordinateCubeIso :: (Integral a, Bits a) => Iso' (CubeCoordinate a) (t a)


class (Functor t) => OffsetCoordinate (t :: * -> *) a where
  offsetCol :: Lens' (t a) a
  offsetRow :: Lens' (t a) a
  _OffsetCoordinate :: Iso' (OffsetBase a) (t a)
  _CubeOffsetIso :: (Integral a, Bits a) => Iso' (CubeCoordinate a) (t a)

newtype CubeCoordinate t =
  CubeCoordinate (t, t, t)
  deriving (Show, Read, Eq, Typeable, GHC.Generic)

newtype AxialCoordinate t =
  AxialCoordinate (t, t)
  deriving (Show, Read, Eq, Typeable, GHC.Generic)

newtype OffsetEvenQ t =
  OffsetEvenQ (t, t)
  deriving (Show, Read, Eq, Typeable, GHC.Generic)

newtype OffsetOddQ t =
  OffsetOddQ (t, t)
  deriving (Show, Read, Eq, Typeable, GHC.Generic)

newtype OffsetEvenR t =
  OffsetEvenR (t, t)
  deriving (Show, Read, Eq, Typeable, GHC.Generic)

newtype OffsetOddR t =
  OffsetOddR (t, t)
  deriving (Show, Read, Eq, Typeable, GHC.Generic)

_CubeCoordinate :: Iso' (CoordinateBase CubeCoordinate t) (CubeCoordinate t)
_CubeCoordinate = iso CubeCoordinate (\(CubeCoordinate a) -> a)

_AxialCoordinate :: Iso' (CoordinateBase AxialCoordinate t) (AxialCoordinate t)
_AxialCoordinate = iso AxialCoordinate (\(AxialCoordinate a) -> a)

_OffsetEvenQ :: Iso' (CoordinateBase OffsetEvenQ t) (OffsetEvenQ t)
_OffsetEvenQ = iso OffsetEvenQ (\(OffsetEvenQ a) -> a)

_OffsetOddQ :: Iso' (CoordinateBase OffsetOddQ t) (OffsetOddQ t)
_OffsetOddQ = iso OffsetOddQ (\(OffsetOddQ a) -> a)

_OffsetEvenR :: Iso' (CoordinateBase OffsetEvenR t) (OffsetEvenR t)
_OffsetEvenR = iso OffsetEvenR (\(OffsetEvenR a) -> a)

_OffsetOddR :: Iso' (CoordinateBase OffsetOddR t) (OffsetOddR t)
_OffsetOddR = iso OffsetOddR (\(OffsetOddR a) -> a)

cubeX :: Lens' (CubeCoordinate t) t
cubeX = lens (\(CubeCoordinate (x,_,_)) -> x)
        (\(CubeCoordinate (_,y,z)) x -> CubeCoordinate (x,y,z))

cubeY :: Lens' (CubeCoordinate t) t
cubeY = lens (\(CubeCoordinate (_,y,_)) -> y)
        (\(CubeCoordinate (x,_,z)) y -> CubeCoordinate (x,y,z))

cubeZ :: Lens' (CubeCoordinate t) t
cubeZ = lens (\(CubeCoordinate (_,_,z)) -> z)
        (\(CubeCoordinate (x,y,_)) z -> CubeCoordinate (x,y,z))


axialRow :: Lens' (AxialCoordinate t) t
axialRow = lens (\(AxialCoordinate (_,r)) -> r)
        (\(AxialCoordinate (q,_)) r -> AxialCoordinate (q,r))

axialCol :: Lens' (AxialCoordinate t) t
axialCol = lens (\(AxialCoordinate (q,_)) -> q)
        (\(AxialCoordinate (_,r)) q -> AxialCoordinate (q,r))


_CubeAxialIso :: (Num t) => Iso' (CubeCoordinate t) (AxialCoordinate t)
_CubeAxialIso =
  let a (CubeCoordinate (x, _, z)) = AxialCoordinate (x, z)
      b (AxialCoordinate (q,r))    = CubeCoordinate  (q, -q-r  ,r)
  in iso a b


_AxialOffsetIso :: (OffsetCoordinate t a, Integral a, Bits a) =>
                  Iso' (AxialCoordinate a) (t a)
_AxialOffsetIso = iso a b
  where a = view ((re _CubeAxialIso) . _CubeOffsetIso)
        b = view ((re _CubeOffsetIso) . _CubeAxialIso)

_CoordinateIso :: (HexCoordinate s a, HexCoordinate t a, Integral a, Bits a) =>
                 Iso' (s a) (t a)
_CoordinateIso = iso a b
  where a = view (re _CoordinateCubeIso . _CoordinateCubeIso)
        b = view (re _CoordinateCubeIso . _CoordinateCubeIso)



instance OffsetCoordinate OffsetEvenQ t where
  offsetCol = lens (\(OffsetEvenQ (c,_)) -> c)
              (\(OffsetEvenQ (_,r)) b -> OffsetEvenQ (b, r))
  offsetRow = lens (\(OffsetEvenQ (_,r)) -> r)
              (\(OffsetEvenQ (c,_)) b -> OffsetEvenQ (c, b))
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
              (\(OffsetOddQ (_,r)) b -> OffsetOddQ (b, r))
  offsetRow = lens (\(OffsetOddQ (_,r)) -> r)
              (\(OffsetOddQ (c,_)) b -> OffsetOddQ (c, b))
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
              (\(OffsetEvenR (_,r)) b -> OffsetEvenR (b, r))
  offsetRow = lens (\(OffsetEvenR (_,r)) -> r)
              (\(OffsetEvenR (c,_)) b -> OffsetEvenR (c, b))
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
              (\(OffsetOddR (_,r)) b -> OffsetOddR (b, r))
  offsetRow = lens (\(OffsetOddR (_,r)) -> r)
              (\(OffsetOddR (c,_)) b -> OffsetOddR (c, b))
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


