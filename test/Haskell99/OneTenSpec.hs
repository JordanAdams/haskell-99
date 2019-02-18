module Haskell99.OneTenSpec (spec) where

import Test.Hspec
import Haskell99.OneTen (last', lastButOne)

spec :: Spec
spec = do
  describe "01: last'" $ do
    it "should get the last item of a list" $ do
      last' [1,2,3,4]       `shouldBe` (4 :: Int)
      last' ['x', 'y', 'z'] `shouldBe` ('z' :: Char)
      last' "xyz"           `shouldBe` ('z' :: Char)

  describe "02: lastButOne" $ do
    it "should get the last but one element of a list" $ do
      lastButOne [1,2,3,4]       `shouldBe` (3 :: Int)
      lastButOne ['x', 'y', 'z'] `shouldBe` ('y' :: Char)
      lastButOne "xyz"           `shouldBe` ('y' :: Char)
