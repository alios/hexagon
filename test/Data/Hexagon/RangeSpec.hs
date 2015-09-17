module Data.Hexagon.RangeSpec where

import           SpecHelper

spec :: Spec
spec = do
  describe "Data.Hexagon.Range" $ do
    context "context" $ do
      it "does something" $ do
        True `shouldBe` True

main :: IO ()
main = hspec spec
