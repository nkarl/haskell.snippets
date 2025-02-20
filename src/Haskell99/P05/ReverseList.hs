module Haskell99.P05.ReverseList where

import Test.HUnit

reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = go xs []
 where
  go [] acc = acc
  go (y : ys) acc = go ys (y : acc)

-- foldl (flip (:)) mempty "abcdefg" >>> "gfedcba"
-- (flip (:)) mempty 'a'
reverseFoldL :: [a] -> [a]
reverseFoldL = foldl (flip (:)) []

data StateQ a = StateQ
  { remained :: [a]
  , reversed :: [a]
  }
  deriving (Show)

-- the same as `foldl`
rev :: forall a. (Show a) => StateQ a -> IO [a]
rev t@(StateQ{remained, reversed}) = case remained of
  [] -> pure reversed
  (x : xs) -> do
    _ <- print t
    rev t{remained = xs, reversed = x : reversed}

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
        , TestCase $ makeTest (someListInt == reverseFoldL someListIntRev) (show someListInt ++ " should be reversed")
        , TestCase $ makeTest (emptyListInt == reverseFoldL emptyListInt) (show emptyListInt ++ " should be reversed")
        , TestCase $ makeTest (someListString == reverseFoldL someListStringRev) (show someListString ++ " should be reverse")
        , TestCase $ makeTest (emptyListString == reverseFoldL emptyListString) (show emptyListString ++ " should be reversed")
        ]
  runTestTT tests
