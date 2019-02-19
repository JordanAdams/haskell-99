module Haskell99.ElevenTwentySpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Haskell99.ElevenTwenty

spec :: Spec
spec = do
  describe "11: typedEncode" $ do
    it "should encode using run-length encoding, but with types" $ do
      typedEncode "aaabcc"              `shouldBe` [Many 3 'a', One 'b', Many 2 'c']
      typedEncode [1, 1, 2, 3, 3, 3, 4] `shouldBe` [Many 2 1, One 2, Many 3 3, One 4]

  describe "12: decode" $ do
    it "should decode a run-length encoded value" $ do
      decode [Many 3 'a', One 'b', Many 2 'c']  `shouldBe` "aaabcc"
      decode [Many 2 1, One 2, Many 3 3, One 4] `shouldBe` [1, 1, 2, 3, 3, 3, 4]

  describe "14: duplicate" $ do
    it "should duplicate items in the given list" $ do
      duplicate []      `shouldBe` ([] :: [Int])
      duplicate [1,2,3] `shouldBe` [1, 1, 2, 2, 3, 3]
      duplicate "abccd" `shouldBe` "aabbccccdd"

  describe "15: repli" $ do
    it "should replicate items in the given list" $ do
      repli 4 ""    `shouldBe` ""
      repli 4 "a"   `shouldBe` "aaaa"
      repli 4 "abc" `shouldBe` "aaaabbbbcccc"

  describe "16: dropEvery" $ do
    it "should drop every nth item in the given list" $ do
      dropEvery 3 []            `shouldBe` ([] :: [Int])
      dropEvery 3 [1,2,3,4,5,6] `shouldBe` [1,2,4,5]
      dropEvery 1 [1,2,3,4,5,6] `shouldBe` []

  describe "17: split" $ do
    it "should split the given list at the specified index" $ do
      split 3 []            `shouldBe` (([], []) :: ([Int], [Int]))
      split 3 [1,2,3,4,5,6] `shouldBe` ([1,2,3], [4,5,6])
      split 1 [1,2,3,4,5,6] `shouldBe` ([1], [2,3,4,5,6])

  describe "18: slice" $ do
    it "should take a slice from the given list" $ do
      slice 3 6 [1,2,3,4,5,6,7,8] `shouldBe` [3,4,5,6]
      slice 3 3 [1,2,3,4,5,6,7,8] `shouldBe` [3]
      slice 1 4 [1,2,3,4,5,6,7,8] `shouldBe` [1,2,3,4]
      slice 0 3 [1,2,3,4,5,6,7,8] `shouldBe` [1,2,3]

  describe "19: rotate" $ do
    it "should rotate the given list" $ do
      rotate 0     [1,2,3]       `shouldBe` [1,2,3]
      rotate 10    []            `shouldBe` ([] :: [Int])
      rotate (-10) []            `shouldBe` ([] :: [Int])
      rotate 10    [1]           `shouldBe` [1]
      rotate (-10) [1]           `shouldBe` [1]
      rotate 2     [1,2,3,4,5,6] `shouldBe` [3,4,5,6,1,2]
      rotate (-2)  [1,2,3,4,5,6] `shouldBe` [5,6,1,2,3,4]

  describe "20: removeAt" $ do
    it "should remove an item in the given list" $ do
      removeAt 1 []          `shouldBe` ([] :: [Int])
      removeAt 10 [1,2,3]    `shouldBe` [1,2,3]
      removeAt 3 [1,2,3,4,5] `shouldBe` [1,2,4,5]

      evaluate (removeAt 0 [1])  `shouldThrow` errorCall "out of bounds"
      evaluate (removeAt (-3) [1]) `shouldThrow` errorCall "out of bounds"
