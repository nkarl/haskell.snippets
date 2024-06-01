module Haskell99.P03.FindkElement where

import Test.HUnit

findkElement :: Int -> [a] -> Maybe a
findkElement _ [] = Nothing
findkElement k (x : xs)
  | k == 0 = Just x
  | otherwise = findkElement (k - 1) xs

testNum :: IO Counts
testNum = do
  let
    x = Just 4 :: Maybe Int
    y = Just 3 :: Maybe Int
    z = Nothing :: Maybe Int
    someList = [1, 2, 3, 4] :: [Int]
    emptyList = [] :: [Int]
    makeTest = flip assertBool
    tests =
      TestList
        [ TestCase (makeTest (x == findkElement 3 someList) (show x ++ " is last of " ++ show someList))
        , TestCase (makeTest (y /= findkElement 3 someList) (show x ++ " is NOT last of " ++ show someList))
        , TestCase (makeTest (z /= findkElement 3 someList) (show z ++ " is NOT last of " ++ show someList))
        , TestCase (makeTest (x /= findkElement 3 emptyList) (show x ++ " is NOT last of " ++ show emptyList))
        , TestCase (makeTest (z == findkElement 3 emptyList) (show z ++ " is last of " ++ show emptyList))
        ]
  runTestTT tests
