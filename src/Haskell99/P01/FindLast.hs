module Haskell99.P01.FindLast where

import Test.HUnit
import Prelude

findLast :: (Show a) => [a] -> Maybe a
findLast [] = Nothing
findLast (x : xs) = Just $ go x xs
 where
  go y [] = y
  go _ (z : zs) = go z zs

findLastNum :: (Show a, Num a) => [a] -> Maybe a
findLastNum = findLast

testNum :: IO Counts
testNum = do
  let
    x = Just 4 :: Maybe Int
    y = Just 3 :: Maybe Int
    nothing = Nothing :: Maybe Int
    someList = [1, 2, 3, 4] :: [Int]
    emptyList = [] :: [Int]
    tests =
      TestList
        [ TestCase
            ( assertBool
                (show x ++ " is last of " ++ show someList)
                (x == findLast someList)
            )
        , TestCase
            ( assertBool
                (show x ++ " is NOT last of " ++ show someList)
                (y /= findLast someList)
            )
        , TestCase
            ( assertBool
                (show nothing ++ " is NOT last of " ++ show someList)
                (nothing /= findLast someList)
            )
        , TestCase
            ( assertBool
                (show x ++ " is NOT last of " ++ show emptyList)
                (x /= findLast emptyList)
            )
        , TestCase
            ( assertBool
                (show nothing ++ " is last of " ++ show emptyList)
                (nothing == findLast emptyList)
            )
        ]
  runTestTT tests
