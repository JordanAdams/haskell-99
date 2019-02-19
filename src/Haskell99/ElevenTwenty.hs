module Haskell99.ElevenTwenty where

import Haskell99.OneTen (encode)

data EncodedItem a = One a | Many Int a
  deriving (Show, Eq)

-- Problem 11:
-- Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.
typedEncode :: (Eq a) => [a] -> [EncodedItem a]
typedEncode = (map mapper) . encode
  where
    mapper (x, 1) = One x
    mapper (x, n) = Many n x


-- Problem 12:
-- Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.
decode :: [EncodedItem a] -> [a]
decode = concatMap mapper
  where
    mapper (One x) = [x]
    mapper (Many n x) = take n (repeat x)


-- Problem 13:
-- Skipped due to unclear definition


-- Problem 14:
-- Duplicate the elements of a list.
duplicate :: [a] -> [a]
duplicate = repli 2


-- Problem 15:
-- Replicate the elements of a list.
repli :: Int -> [a] -> [a]
repli n [] = []
repli n (x:xs) = take n (repeat x) ++ repli n xs


-- Problem 16:
-- Drop every N'th element from a list.
dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = head ++ dropEvery n rest
  where
    head = take (n - 1) xs
    rest = drop n xs


-- Problem 17:
-- Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
split _ [] = ([], [])
split 0 xs = ([], xs)
split n (x:xs) = (x : l, r)
  where (l, r) = split (n - 1) xs


-- Problem 18:
-- Extract a slice from a list.
--
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits included).
--
-- Start counting the elements with 1.
slice :: Int -> Int -> [a] -> [a]
slice i k = drop (i-1) . fst . splitAt k


-- Problem 19:
-- Rotate a list N places to the left.
rotate :: Int -> [a] -> [a]
rotate 0 xs = xs
rotate _ [] = []
rotate n xs
  | n > 0 = rotate (n-1) (tail xs ++ [head xs])
  | n < 0 = rotate (n+1) (last xs : init xs)


-- Problem 20:
-- Remove the K'th element from a list.
removeAt :: Int -> [a] -> [a]
removeAt n xs
  | n < 1     = error "out of bounds"
  | otherwise = l ++ drop 1 r
  where
    (l, r) = splitAt (n-1) xs

-- Or using recursion:
-- removeAt n xs
--   | null xs   = xs
--   | n < 1     = error "out of bounds"
--   | n == 1    = tail xs
--   | otherwise = head xs : removeAt (n-1) (tail xs)
