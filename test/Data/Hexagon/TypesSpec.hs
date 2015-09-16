module Data.Hexagon.TypesSpec where

import SpecHelper


spec :: Spec
spec = do
  describe "Data.Hexagon.Types" $ do
    context "context" $ do
      it "does something" $ do
        True `shouldBe` True


main :: IO ()
main = hspec spec
