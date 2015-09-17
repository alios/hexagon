{-# LANGUAGE FlexibleInstances #-}

module Data.Hexagon.TypesSpec where

import           Control.Lens
import           SpecHelper



spec :: Spec
spec = do
  describe "Data.Hexagon.Types" $ do
    context "CubeCoordinate" $ do
      it "_CubeCoordinate is isomorph" $ property $ prop_cube_iso
      it "_CubeAxialIso is isomorph" $ property $ prop_cube_axial_iso
      it "_CubeOffsetIso is isomorph with OffsetEvenQ" $ property $ prop_cube_offsetEvenQ_iso
      it "_CubeOffsetIso is isomorph with OffsetEvenR" $ property $ prop_cube_offsetEvenR_iso
      it "_CubeOffsetIso is isomorph with OffsetOddQ" $ property $ prop_cube_offsetOddQ_iso
      it "_CubeOffsetIso is isomorph with OffsetOddR" $ property $ prop_cube_offsetOddR_iso
    context "AxialCoordinate" $ do
      it "_AxialCoordinate is isomorph" $ property $ prop_axial_iso
      it "_CubeAxialIso is isomorph" $ property $ prop_axial_cube_iso
      it "_AxialOffsetIso is isomorph with OffsetEvenQ" $ property $ prop_axial_offsetEvenQ_iso
      it "_AxialOffsetIso is isomorph with OffsetEvenR" $ property $ prop_axial_offsetEvenR_iso
      it "_AxialOffsetIso is isomorph with OffsetOddQ" $ property $ prop_axial_offsetOddQ_iso
      it "_AxialOffsetIso is isomorph with OffsetOddR" $ property $ prop_axial_offsetOddR_iso
    context "OffsetEvenQ" $ do
      it "_OffsetCoordinate is isomproph" $ property $ prop_offset_eq_iso
      it "_AxialOffsetIso is isomorph" $ property $ prop_offsetEvenQ_axial_iso
      it "_CubeOffsetIso is isomorph" $ property $ prop_offsetEvenQ_cube_iso

    context "OffsetEvenR" $ do
      it "_OffsetCoordinate is isomproph" $ property $ prop_offset_er_iso
      it "_AxialOffsetIso is isomorph" $ property $ prop_offsetEvenR_axial_iso
      it "_CubeOffsetIso is isomorph" $ property $ prop_offsetEvenR_cube_iso


prop_offset_eq_iso :: OffsetEvenQ Int -> Bool
prop_offset_eq_iso c = c == (view (_OffsetCoordinate . from _OffsetCoordinate ) c)

prop_offsetEvenQ_axial_iso :: OffsetEvenQ Int -> Bool
prop_offsetEvenQ_axial_iso c =
  c == (view (from _AxialOffsetIso . _AxialOffsetIso) c)


prop_offsetEvenQ_cube_iso :: OffsetEvenQ Int -> Bool
prop_offsetEvenQ_cube_iso c =
  c == (view (from _CubeOffsetIso . _CubeOffsetIso) c)


prop_offset_er_iso :: OffsetEvenR Int -> Bool
prop_offset_er_iso c = c == (view (_OffsetCoordinate . from _OffsetCoordinate ) c)

prop_offsetEvenR_axial_iso :: OffsetEvenR Int -> Bool
prop_offsetEvenR_axial_iso c =
  c == (view (from _AxialOffsetIso . _AxialOffsetIso) c)

prop_offsetEvenR_cube_iso :: OffsetEvenR Int -> Bool
prop_offsetEvenR_cube_iso c =
  c == (view (from _CubeOffsetIso . _CubeOffsetIso) c)



prop_cube_iso :: CubeCoordinate Int -> Bool
prop_cube_iso c = c == (view (_CubeCoordinate . from _CubeCoordinate) c)

prop_axial_iso :: AxialCoordinate Int -> Bool
prop_axial_iso c = c == (view (_AxialCoordinate . from _AxialCoordinate) c)

prop_cube_axial_iso :: CubeCoordinate Int -> Bool
prop_cube_axial_iso c = c == (view (_CubeAxialIso . from _CubeAxialIso) c)

prop_axial_cube_iso :: AxialCoordinate Int -> Bool
prop_axial_cube_iso c = c == (view (from _CubeAxialIso . _CubeAxialIso) c)



prop_cube_offsetEvenQ_iso :: CubeCoordinate Int -> Bool
prop_cube_offsetEvenQ_iso c =
  let c' :: OffsetEvenQ Int
      c' = view (_CubeOffsetIso) c
  in c == (view (from _CubeOffsetIso) c')


prop_cube_offsetEvenR_iso :: CubeCoordinate Int -> Bool
prop_cube_offsetEvenR_iso c =
  let c' :: OffsetEvenR Int
      c' = view (_CubeOffsetIso) c
  in c == (view (from _CubeOffsetIso) c')


prop_cube_offsetOddQ_iso :: CubeCoordinate Int -> Bool
prop_cube_offsetOddQ_iso c =
  let c' :: OffsetOddQ Int
      c' = view (_CubeOffsetIso) c
  in c == (view (from _CubeOffsetIso) c')


prop_cube_offsetOddR_iso :: CubeCoordinate Int -> Bool
prop_cube_offsetOddR_iso c =
  let c' :: OffsetOddR Int
      c' = view (_CubeOffsetIso) c
  in c == (view (from _CubeOffsetIso) c')

prop_axial_offsetEvenQ_iso :: AxialCoordinate Int -> Bool
prop_axial_offsetEvenQ_iso c =
  let c' :: OffsetEvenQ Int
      c' = view (_AxialOffsetIso) c
  in c == (view (from _AxialOffsetIso) c')


prop_axial_offsetEvenR_iso :: AxialCoordinate Int -> Bool
prop_axial_offsetEvenR_iso c =
  let c' :: OffsetEvenR Int
      c' = view (_AxialOffsetIso) c
  in c == (view (from _AxialOffsetIso) c')


prop_axial_offsetOddQ_iso :: AxialCoordinate Int -> Bool
prop_axial_offsetOddQ_iso c =
  let c' :: OffsetOddQ Int
      c' = view (_AxialOffsetIso) c
  in c == (view (from _AxialOffsetIso) c')


prop_axial_offsetOddR_iso :: AxialCoordinate Int -> Bool
prop_axial_offsetOddR_iso c =
  let c' :: OffsetOddR Int
      c' = view (_AxialOffsetIso) c
  in c == (view (from _AxialOffsetIso) c')


main :: IO ()
main = hspec spec
