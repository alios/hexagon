module Data.Hexagon.NeighborsSpec where

import SpecHelper

spec :: Spec
spec = do
  describe "Data.Hexagon.Neighbors" $ do
    context "context" $ do
      it "does something" $ do
        True `shouldBe` True


main :: IO ()
main = hspec spec
