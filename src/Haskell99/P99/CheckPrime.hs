module Haskell99.P99.CheckPrime where

import GHC.Float (float2Int, int2Float)
import Test.HUnit
import Prelude

isPrime :: Int -> Bool
isPrime 2 = True
isPrime num
  | even num = False
  | otherwise = checkPrime num 3
 where
  runSqrt = float2Int . sqrt . int2Float
  sqr = runSqrt num
  checkPrime n k
    | k > sqr = True
    | n `mod` k == 0 = False
    | otherwise = checkPrime n (k + 2)

test :: IO Counts
test = do
  let
    makeTest = flip assertBool
    tests =
      TestList
        [ let num = 2 :: Int
           in TestCase $ makeTest (isPrime num) (show num <> " should be prime")
        ]
  runTestTT tests
