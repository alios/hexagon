module Data.Hexagon.LineDrawSpec where

import SpecHelper

spec :: Spec
spec = do
  describe "Data.Hexagon.LineDraw" $ do
    context "context" $ do
      it "does something" $ do
        True `shouldBe` True

main :: IO ()
main = hspec spec
