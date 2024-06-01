module Haskell99.P02.FindButLast where

import Prelude

import Test.HUnit

findButLast :: [a] -> Maybe a
findButLast [] = Nothing
findButLast [x, _] = Just x
findButLast (_ : xs) = findButLast xs

testNum :: IO Counts
testNum = do
  let
    x = Just 3 :: Maybe Int
    y = Just 4 :: Maybe Int
    z = Nothing :: Maybe Int
    someList = [1, 2, 3, 4] :: [Int]
    emptyList = [] :: [Int]
    makeTest = flip assertBool
    tests =
      TestList
        [ TestCase (makeTest (x == findButLast someList) (show x ++ " is last of " ++ show someList))
        , TestCase (makeTest (y /= findButLast someList) (show x ++ " is NOT last of " ++ show someList))
        , TestCase (makeTest (z /= findButLast someList) (show z ++ " is NOT last of " ++ show someList))
        , TestCase (makeTest (x /= findButLast emptyList) (show x ++ " is NOT last of " ++ show emptyList))
        , TestCase (makeTest (z == findButLast emptyList) (show z ++ " is last of " ++ show emptyList))
        ]
  runTestTT tests
