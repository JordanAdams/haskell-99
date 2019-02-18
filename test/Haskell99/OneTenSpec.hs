module Haskell99.OneTenSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)
import Haskell99.OneTen

spec :: Spec
spec = do
  describe "01: last'" $ do
    it "should get the last item of a list" $ do
      last' [1,2,3,4]       `shouldBe` 4
      last' ['x', 'y', 'z'] `shouldBe` 'z'
      last' "xyz"           `shouldBe` 'z'

  describe "02: lastButOne" $ do
    it "should get the last but one element of a list" $ do
      lastButOne [1,2,3,4]       `shouldBe` 3
      lastButOne ['x', 'y', 'z'] `shouldBe` 'y'
      lastButOne "xyz"           `shouldBe` 'y'

  describe "03: elementAt" $ do
    it "should get the element at the given index in a list" $ do
      evaluate (elementAt 2 [])  `shouldThrow` errorCall "out of bounds"
      evaluate (elementAt 2 [1]) `shouldThrow` errorCall "out of bounds"

      elementAt 1 [1]     `shouldBe` 1 
      elementAt 2 [1,2,3] `shouldBe` 2 
      elementAt 2 "xyz"   `shouldBe` 'y'

  describe "04: myLength" $ do
    it "should get the length of the given list" $ do
      myLength []          `shouldBe` 0
      myLength [1,2,3,4,5] `shouldBe` 5
      myLength "abc"       `shouldBe` 3

  describe "05: myReverse" $ do
    it "should reverse the given list" $ do
      myReverse []          `shouldBe` ([] :: [Int])
      myReverse [1,2,3,4,5] `shouldBe` [5,4,3,2,1]
      myReverse "abc"       `shouldBe` "cba"

  describe "06: isPalindrome" $ do
    it "should check if the given list is a palindrome" $ do
      isPalindrome ([] :: [Int]) `shouldBe` True
      isPalindrome [1,2,2,1]     `shouldBe` True
      isPalindrome "madamimadam" `shouldBe` True

      isPalindrome [1,2,3]          `shouldBe` False
      isPalindrome "notapalindrome" `shouldBe` False

  describe "07: flatten" $ do
    it "should flatten a nested list" $ do
      flatten (List [(Elem 1), (List [(Elem 2), (Elem 3)])]) `shouldBe` [1,2,3]

  describe "08: compress" $ do
    it "should remove consecutive duplicates in the given list" $ do
      compress []                    `shouldBe` ([] :: [Int])
      compress [1,2,2,2,3,4,4,5]     `shouldBe` [1,2,3,4,5]
      compress "aaabbccccccdddddcee" `shouldBe` "abcdce"

  describe "09: pack" $ do
    it "should combine consecutive duplicates into sublists" $ do
      pack ""                    `shouldBe` []
      pack "aaabbccccccdddddcee" `shouldBe` ["aaa", "bb", "cccccc", "ddddd", "c", "ee"]

  describe "10: encode" $ do
    it "should encode using run-length encoding" $ do
      encode ""                    `shouldBe` []
      encode "aaabbccccccdddddcee" `shouldBe` [('a', 3), ('b', 2), ('c', 6), ('d', 5), ('c', 1), ('e', 2)]
