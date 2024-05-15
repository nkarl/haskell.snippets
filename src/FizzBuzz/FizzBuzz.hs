module FizzBuzz.FizzBuzz where

import Prelude

data FizzBuzz = Fizz | Buzz | FizzBuzz | Value Int deriving (Show)

fizzle :: Int -> FizzBuzz
fizzle x
  | x `mod` 3 == 0 && x `mod` 5 == 0 = FizzBuzz
  | x `mod` 5 == 0 = Buzz
  | x `mod` 3 == 0 = Fizz
  | otherwise = Value x

data Result
  = Result {iter :: Int, result :: FizzBuzz}
  deriving (Show)

run :: Int -> IO ()
run x =
  go 1 x
 where
  go acc 1 =
    print $ Result{iter = acc, result = fizzle acc}
  go acc n = do
    print $ Result{iter = acc, result = fizzle acc}
    go (acc + 1) (n - 1)

test :: IO ()
test = do
  print $ "Header: run 3"
  run 3
  print $ "Header: run 5"
  run 5
  print $ "Header: run 15"
  run 15
