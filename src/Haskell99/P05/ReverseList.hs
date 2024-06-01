module Haskell99.P05.ReverseList where

import Test.HUnit

reverse' :: [a] -> [a]
reverse' [] = []
reverse' ls = go ls []
 where
  go [] acc = acc
  go (x : xs) acc = go xs (x : acc)

test :: IO Counts
test = do
  let
    someListInt = [1, 2, 3, 4] :: [Int]
    someListIntRev = [4, 3, 2, 1] :: [Int]
    emptyListInt = [] :: [Int]
    someListString = ["aa", "bb", "cc", ""] :: [String]
    someListStringRev = ["", "cc", "bb", "aa"] :: [String]
    emptyListString = [] :: [String]
    makeTest = flip assertBool
    tests =
      TestList
        [ TestCase $ makeTest (someListInt == reverse' someListIntRev) (show someListInt ++ " should be reversed")
        , TestCase $ makeTest (emptyListInt == reverse' emptyListInt) (show emptyListInt ++ " should be reversed")
        , TestCase $ makeTest (someListString == reverse' someListStringRev) (show someListString ++ " should be reverse")
        , TestCase $ makeTest (emptyListString == reverse' emptyListString) (show emptyListString ++ " should be reversed")
        ]
  runTestTT tests
