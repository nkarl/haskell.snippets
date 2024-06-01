module Haskell99.P01.FindLast where

import Test.HUnit
import Prelude

findLast :: (Show a) => [a] -> Maybe a
findLast [] = Nothing
findLast (x : xs) = Just $ go x xs
 where
  go y [] = y
  go _ (z : zs) = go z zs

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
        [ TestCase (makeTest (x == findLast someList) (show x ++ " is last of " ++ show someList))
        , TestCase (makeTest (y /= findLast someList) (show x ++ " is NOT last of " ++ show someList))
        , TestCase (makeTest (z /= findLast someList) (show z ++ " is NOT last of " ++ show someList))
        , TestCase (makeTest (x /= findLast emptyList) (show x ++ " is NOT last of " ++ show emptyList))
        , TestCase (makeTest (z == findLast emptyList) (show z ++ " is last of " ++ show emptyList))
        ]
  runTestTT tests
