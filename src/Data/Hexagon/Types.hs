{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}

module Data.Hexagon.Types where

import           Control.Lens
import           Data.Bits
import           Data.Typeable (Typeable)
import qualified GHC.Generics  as GHC

class (Functor t) => OffsetCoordinate (t :: * -> *) a where
  offsetCol :: Lens' (t a) a
  offsetRow :: Lens' (t a) a
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
axialRow = lens (\(AxialCoordinate (q,r)) -> r)
        (\(AxialCoordinate (q,_)) r -> AxialCoordinate (q,r))

axialCol :: Lens' (AxialCoordinate t) t
axialCol = lens (\(AxialCoordinate (q,r)) -> q)
        (\(AxialCoordinate (_,r)) q -> AxialCoordinate (q,r))


_CubeAxialIso :: (Num t) => Iso' (CubeCoordinate t) (AxialCoordinate t)
_CubeAxialIso =
  let a (CubeCoordinate (x, _, z)) = AxialCoordinate (x, z)
      b (AxialCoordinate (q,r))    = CubeCoordinate  (q, -q-r  ,r)
  in iso a b


foo :: (OffsetCoordinate t a, Integral a, Bits a) => Iso' (AxialCoordinate a) (t a)
foo = undefined -- _CubeAxialIso . _CubeOffsetIso

instance OffsetCoordinate OffsetEvenQ t where
  offsetCol = lens (\(OffsetEvenQ (c,r)) -> c) (\(OffsetEvenQ (c,r)) b -> OffsetEvenQ (b, r))
  offsetRow = lens (\(OffsetEvenQ (c,r)) -> r) (\(OffsetEvenQ (c,r)) b -> OffsetEvenQ (c, b))
  _CubeOffsetIso =
    let a (CubeCoordinate (x,y,z)) = OffsetEvenQ (x, z + (x + (x .&. 1)) `div` 2)
        b (OffsetEvenQ (c, r))     =
          let x = c
              z = r - (c + (c .&. 1)) `div` 2
              y = -x-z
          in CubeCoordinate (x, y, z)
    in iso a b


instance OffsetCoordinate OffsetOddQ t where
  offsetCol = lens (\(OffsetOddQ (c,r)) -> c) (\(OffsetOddQ (c,r)) b -> OffsetOddQ (b, r))
  offsetRow = lens (\(OffsetOddQ (c,r)) -> r) (\(OffsetOddQ (c,r)) b -> OffsetOddQ (c, b))
  _CubeOffsetIso =
      let a (CubeCoordinate (x,y,z)) =
            OffsetOddQ (x, z + (x - (x .&. 1)) `div` 2)
          b (OffsetOddQ (c, r))     =
            let x = c
                z = r - (c - (c .&. 1)) `div` 2
                y = -x-z
            in CubeCoordinate (x, y, z)
      in iso a b

instance OffsetCoordinate OffsetEvenR t where
  offsetCol = lens (\(OffsetEvenR (c,r)) -> c) (\(OffsetEvenR (c,r)) b -> OffsetEvenR (b, r))
  offsetRow = lens (\(OffsetEvenR (c,r)) -> r) (\(OffsetEvenR (c,r)) b -> OffsetEvenR (c, b))
  _CubeOffsetIso =
    let a (CubeCoordinate (x,y,z)) =
          OffsetEvenR (x + (z + (z .&. 1)) `div` 2, z)
        b (OffsetEvenR (c, r))     =
          let x = c - (r + (r .&. 1)) `div` 2
              z = r
              y = -x-z
          in CubeCoordinate (x, y, z)
    in iso a b


instance OffsetCoordinate OffsetOddR t where
  offsetCol = lens (\(OffsetOddR (c,r)) -> c) (\(OffsetOddR (c,r)) b -> OffsetOddR (b, r))
  offsetRow = lens (\(OffsetOddR (c,r)) -> r) (\(OffsetOddR (c,r)) b -> OffsetOddR (c, b))
  _CubeOffsetIso =
    let a (CubeCoordinate (x,y,z)) = OffsetOddR (x + (z - (z .&. 1)) `div` 2, z)
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
