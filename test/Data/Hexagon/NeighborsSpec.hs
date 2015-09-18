module Data.Hexagon.NeighborsSpec where

import qualified Data.Sequence as Seq
import SpecHelper

spec :: Spec
spec = do
  describe "Data.Hexagon.Neighbors" $ do
    context "every HexCoordinate has 6 neighbors" $ do
      it "CubeCoordinate" $ property $ prop_neighbor_count_cube
      it "AxialCoordinate" $ property $ prop_neighbor_count_axial
      it "OffsetEvenQ" $ property $ prop_neighbor_count_evenq
      it "OffsetOddQ" $ property $ prop_neighbor_count_oddq
      it "OffsetEvenR" $ property $ prop_neighbor_count_evenr
      it "OffserOddR" $ property $ prop_neighbor_count_oddr
    context "every neighbor of a node has a distance of 1 to that node" $ do
      it "CubeCoordinate" $ property $ prop_neighbor_dist_cube
      it "AxialCoordinate" $ property $ prop_neighbor_dist_axial
      it "OffsetEvenQ" $ property $ prop_neighbor_dist_evenq
      it "OffsetOddQ" $ property $ prop_neighbor_dist_oddq
      it "OffsetEvenR" $ property $ prop_neighbor_dist_evenr
      it "OffsetOddR" $ property $ prop_neighbor_dist_oddr
    context "every neighbor is within range 1" $ do
      it "CubeCoordinate" $ property $ prop_neighbor_range_cube
      it "AxialCoordinate" $ property $ prop_neighbor_range_axial
      it "OffsetEvenQ" $ property $ prop_neighbor_range_evenq
      it "OffsetOddQ" $ property $ prop_neighbor_range_oddq
      it "OffsetEvenR" $ property $ prop_neighbor_range_evenr
      it "OffsetOddR" $ property $ prop_neighbor_range_oddr
    context "lineDraw from c to neighbor has 2 elements, c and neighbor" $ do
      it "CubeCoordinate" $ property $ prop_neighbor_lineDraw_cube
      it "AxialCoordinate" $ property $ prop_neighbor_lineDraw_axial
      it "OffsetEvenQ" $ property $ prop_neighbor_lineDraw_evenq
      it "OffsetOddQ" $ property $ prop_neighbor_lineDraw_oddq
      it "OffsetEvenR" $ property $ prop_neighbor_lineDraw_evenr
      it "OffsetOddR" $ property $ prop_neighbor_lineDraw_oddr
    context "a neighbor of a coordinate must not be a diagonal of it" $ do
      it "CubeCoordinate" $ property $ prop_neighbor_diagonals_cube
      it "AxialCoordinate" $ property $ prop_neighbor_diagonals_axial
      it "OffsetEvenQ" $ property $ prop_neighbor_diagonals_evenq
      it "OffsetOddQ" $ property $ prop_neighbor_diagonals_oddq
      it "OffsetEvenR" $ property $ prop_neighbor_diagonals_evenr
      it "OffsetOddR" $ property $ prop_neighbor_diagonals_oddr


prop_neighbor_count :: (HasNeighbors t a, Integral a) => t a -> Bool
prop_neighbor_count c = 6 == (length . directions $ c)

prop_neighbor_dist  :: (HexCoordinate t a, HasNeighbors t a, Integral a) =>
                      t a -> Bool
prop_neighbor_dist c = and . fmap ((==) 1 . distance c) . directions $ c


prop_neighbor_range  ::
  (Eq (t a), HexCoordinate t a, HasNeighbors t a, Integral a) =>
  t a -> Bool
prop_neighbor_range c =
  let rs1 = range 1 c
  in and $ fmap (flip elem rs1) . directions $ c

prop_neighbor_diagonals  ::
  (Eq (t a), HexCoordinate t a, HasNeighbors t a, Integral a) =>
  t a -> Bool
prop_neighbor_diagonals c =
  and . fmap (flip notElem (diagonals c)) . directions $ c


prop_neighbor_lineDraw  ::
  (Eq (t a), HexCoordinate t a, HasNeighbors t a, Integral a) =>
  t a -> Bool
prop_neighbor_lineDraw c = and $ fmap (ld c) . directions $ c 
  where ld a b =
          let sq = lineDraw a b
          in (Seq.length sq == 2) && (Seq.index sq 0 == a) && (Seq.index sq 1 == b)





prop_neighbor_count_cube :: CubeCoordinate Int -> Bool
prop_neighbor_count_cube = prop_neighbor_count

prop_neighbor_dist_cube :: CubeCoordinate Int -> Bool
prop_neighbor_dist_cube = prop_neighbor_count

prop_neighbor_range_cube :: CubeCoordinate Int -> Bool
prop_neighbor_range_cube = prop_neighbor_range

prop_neighbor_lineDraw_cube :: CubeCoordinate Int -> Bool
prop_neighbor_lineDraw_cube = prop_neighbor_lineDraw

prop_neighbor_diagonals_cube :: CubeCoordinate Int -> Bool
prop_neighbor_diagonals_cube = prop_neighbor_lineDraw


prop_neighbor_count_axial :: AxialCoordinate Int -> Bool
prop_neighbor_count_axial = prop_neighbor_count

prop_neighbor_dist_axial :: AxialCoordinate Int -> Bool
prop_neighbor_dist_axial = prop_neighbor_count

prop_neighbor_range_axial :: AxialCoordinate Int -> Bool
prop_neighbor_range_axial = prop_neighbor_range

prop_neighbor_lineDraw_axial :: AxialCoordinate Int -> Bool
prop_neighbor_lineDraw_axial = prop_neighbor_lineDraw

prop_neighbor_diagonals_axial :: AxialCoordinate Int -> Bool
prop_neighbor_diagonals_axial = prop_neighbor_lineDraw


prop_neighbor_count_evenq :: OffsetEvenQ Int -> Bool
prop_neighbor_count_evenq = prop_neighbor_count

prop_neighbor_dist_evenq :: OffsetEvenQ Int -> Bool
prop_neighbor_dist_evenq = prop_neighbor_count

prop_neighbor_range_evenq :: OffsetEvenQ Int -> Bool
prop_neighbor_range_evenq = prop_neighbor_range

prop_neighbor_lineDraw_evenq :: OffsetEvenQ Int -> Bool
prop_neighbor_lineDraw_evenq = prop_neighbor_lineDraw

prop_neighbor_diagonals_evenq :: OffsetEvenQ Int -> Bool
prop_neighbor_diagonals_evenq = prop_neighbor_lineDraw

prop_neighbor_count_oddq :: OffsetOddQ Int -> Bool
prop_neighbor_count_oddq = prop_neighbor_count

prop_neighbor_dist_oddq :: OffsetOddQ Int -> Bool
prop_neighbor_dist_oddq = prop_neighbor_count

prop_neighbor_range_oddq :: OffsetOddQ Int -> Bool
prop_neighbor_range_oddq = prop_neighbor_range

prop_neighbor_lineDraw_oddq :: OffsetOddQ Int -> Bool
prop_neighbor_lineDraw_oddq = prop_neighbor_lineDraw

prop_neighbor_diagonals_oddq :: OffsetOddQ Int -> Bool
prop_neighbor_diagonals_oddq = prop_neighbor_lineDraw


prop_neighbor_count_evenr :: OffsetEvenR Int -> Bool
prop_neighbor_count_evenr = prop_neighbor_count

prop_neighbor_dist_evenr :: OffsetEvenR Int -> Bool
prop_neighbor_dist_evenr = prop_neighbor_count

prop_neighbor_range_evenr :: OffsetEvenR Int -> Bool
prop_neighbor_range_evenr = prop_neighbor_range

prop_neighbor_lineDraw_evenr :: OffsetEvenR Int -> Bool
prop_neighbor_lineDraw_evenr = prop_neighbor_lineDraw

prop_neighbor_diagonals_evenr :: OffsetEvenR Int -> Bool
prop_neighbor_diagonals_evenr = prop_neighbor_lineDraw

prop_neighbor_count_oddr :: OffsetOddR Int -> Bool
prop_neighbor_count_oddr = prop_neighbor_count

prop_neighbor_dist_oddr :: OffsetOddR Int -> Bool
prop_neighbor_dist_oddr = prop_neighbor_count

prop_neighbor_range_oddr :: OffsetOddR Int -> Bool
prop_neighbor_range_oddr = prop_neighbor_range

prop_neighbor_lineDraw_oddr :: OffsetOddR Int -> Bool
prop_neighbor_lineDraw_oddr = prop_neighbor_lineDraw

prop_neighbor_diagonals_oddr :: OffsetOddR Int -> Bool
prop_neighbor_diagonals_oddr = prop_neighbor_lineDraw


main :: IO ()
main = hspec spec
