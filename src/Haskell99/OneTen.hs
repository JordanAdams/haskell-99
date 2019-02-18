module Haskell99.OneTen where

-- Problem 1:
-- Find the last element of a list.
last' :: [a] -> a
last' = head . reverse

-- Problem 2:
-- Find the last but one element of a list.
lastButOne :: [a] -> a
lastButOne = head . drop 1 . reverse
