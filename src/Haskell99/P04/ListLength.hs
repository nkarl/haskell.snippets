module Haskell99.P04.ListLength where

import Test.HUnit
import Prelude

length' :: [a] -> Int
length' ls = go ls 0
 where
  go :: [a] -> Int -> Int
  go [] acc = acc
  go (_ : xs) acc = go xs (acc + 1)

test :: IO Counts
test = do
  let
    (someLength, zeroLength) = (4, 0)
    someListInt = [1, 2, 3, 4] :: [Int]
    emptyListInt = [] :: [Int]
    someListString = ["aa", "bb", "cc", ""] :: [String]
    emptyListString = [] :: [String]
    makeTest = flip assertBool
    tests =
      TestList
        [ TestCase (makeTest (someLength == length' someListInt) (show someLength ++ " is length of " ++ show someListInt))
        , TestCase (makeTest (zeroLength == length' emptyListInt) (show zeroLength ++ " is length of " ++ show emptyListInt))
        , TestCase (makeTest (someLength == length' someListString) (show someLength ++ " is length of " ++ show someListString))
        , TestCase (makeTest (zeroLength == length' emptyListString) (show zeroLength ++ " is length of " ++ show emptyListString))
        ]
  runTestTT tests
