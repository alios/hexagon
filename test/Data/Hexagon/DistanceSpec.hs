module Data.Hexagon.DistanceSpec where

import SpecHelper

spec :: Spec
spec = do
  describe "Data.Hexagon.Distance" $ do
    context "context" $ do
      it "does something" $ do
        True `shouldBe` True

main :: IO ()
main = hspec spec
