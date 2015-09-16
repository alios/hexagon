module Data.Hexagon.DiagonalsSpec where

import SpecHelper

spec :: Spec
spec = do
  describe "Data.Hexagon.Diagonals" $ do
    context "context" $ do
      it "does something" $ do
        True `shouldBe` True



main :: IO ()
main = hspec spec
