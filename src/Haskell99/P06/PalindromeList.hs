module Haskell99.P06.PalindromeList where

import Test.HUnit
import Prelude

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

type Reduce t = t -> Bool

isPalindromeA :: (Eq a) => Reduce [a]
isPalindromeA = (==) <*> reverse

isPalindromeM :: (Eq a) => Reduce [a]
isPalindromeM = reverse >>= (==)

-- NOTE: two pointers with an accumulator (reversed)
isPalindromeHalf :: (Eq a) => [a] -> Bool
isPalindromeHalf ls = compare' [] ls ls
 where
  compare' _ [] _ = True
  compare' acc xs [] = acc == xs
  compare' acc (_ : xs) [_] = acc == xs
  compare' acc (x : xs) (_ : _ : ys) = compare' (x : acc) xs ys

test :: IO Counts
test = do
  let
    someListInt = [1, 2, 3, 4] :: [Int]
    emptyListInt = [] :: [Int]
    someListString = ["aa", "bb", "cc", ""] :: [String]
    emptyListString = [] :: [String]
    someString = "abcde"
    emptyString = ""
    listInt = [1, 2, 3, 2, 1] :: [Int]
    listString = ["aa", "bb", "aa"] :: [String]
    string = "abcba"
    makeTest = flip assertBool
    tests =
      TestList
        [ TestCase $ makeTest (not $ isPalindrome someListInt) (show someListInt ++ "should NOT be palindrome")
        , TestCase $ makeTest (isPalindrome emptyListInt) (show emptyListInt ++ "should be palindrome")
        , TestCase $ makeTest (not $ isPalindrome someListString) (show someListString ++ "should NOT be palindrome")
        , TestCase $ makeTest (isPalindrome emptyListString) (show emptyListString ++ "should be palindrome")
        , TestCase $ makeTest (not $ isPalindrome someString) (show someString ++ "should NOT be palindrome")
        , TestCase $ makeTest (isPalindrome emptyString) (show emptyString ++ "should be palindrome")
        , TestCase $ makeTest (isPalindrome listInt) (show listInt ++ "should be ")
        , TestCase $ makeTest (isPalindrome listString) (show listString ++ "should be ")
        , TestCase $ makeTest (isPalindrome string) (show string ++ "should be ")
        , -- using the two-pointer method
          TestCase $ makeTest (not $ isPalindromeHalf someListInt) (show someListInt ++ "should NOT be palindrome")
        , TestCase $ makeTest (isPalindromeHalf emptyListInt) (show emptyListInt ++ "should be palindrome")
        , TestCase $ makeTest (not $ isPalindromeHalf someListString) (show someListString ++ "should NOT be palindrome")
        , TestCase $ makeTest (isPalindromeHalf emptyListString) (show emptyListString ++ "should be palindrome")
        , TestCase $ makeTest (not $ isPalindromeHalf someString) (show someString ++ "should NOT be palindrome")
        , TestCase $ makeTest (isPalindromeHalf emptyString) (show emptyString ++ "should be palindrome")
        , TestCase $ makeTest (isPalindromeHalf listInt) (show listInt ++ "should be ")
        , TestCase $ makeTest (isPalindromeHalf listString) (show listString ++ "should be ")
        , TestCase $ makeTest (isPalindromeHalf string) (show string ++ "should be ")
        ]
  runTestTT tests
