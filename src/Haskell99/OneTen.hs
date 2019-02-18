module Haskell99.OneTen where

import Data.List (group, map)

-- Problem 1:
-- Find the last element of a list.
last' :: [a] -> a
last' = head . reverse


-- Problem 2:
-- Find the last but one element of a list.
lastButOne :: [a] -> a
lastButOne = head . drop 1 . reverse


-- Problem 3:
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: Int -> [a] -> a
elementAt _ []    = error "out of bounds"
elementAt 1 (x:_) = x
elementAt n (x:xs)
  | n > 1     = elementAt (n - 1) xs
  | otherwise = error "out of bounds"


-- Problem 4:
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength xs = sum [1 | _ <- xs]

-- Or with recursion:
-- myLength [] = 0
-- myLength (_:xs) = 1 + length xs


-- Problem 5:
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]


-- Problem 6:
-- Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (reverse x) == x


-- Problem 7:
-- Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (Elem x) = [x]


-- Problem 8:
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
compress :: (Eq a) => [a] -> [a]
compress = (map head) . group

-- Or without group:
-- compress [] = []
-- compress [x] = [x]
-- compress (x:(y:ys))
--   | x == y = compress (y : ys)
--   | otherwise = x : compress (y : ys)


-- Problem 9:
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
pack :: [Char] -> [[Char]]
pack "" = []
pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)


-- Problem 10:
-- Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: [Char] -> [(Char, Int)]
encode = map (\x -> (head x, length x)) . pack
