module Library.Foldable where

import Test.HUnit
import Text.Pretty.Simple (pPrint)
import Prelude hiding (Foldable, foldl, foldr)

class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b

  -- | maps each element of the structure into a monoid, and combine the results with `(<>)`. Right associative.
  foldMR :: forall a m. (Monoid m) => (a -> m) -> f a -> m

  foldML :: forall a m. (Monoid m) => (a -> m) -> f a -> m

instance Foldable [] where
  foldr _ a [] = a
  foldr f a (x : xs) = f x (foldr f a xs)
  foldl _ a [] = a
  foldl f a (x : xs) = foldl f (f a x) xs
  foldMR f = foldr ((<>) . f) mempty

  -- \| Left associative.
  foldML f = foldl g mempty
   where
    g x = (<>) x . f

foldRepR :: Int -> [a] -> [a]
foldRepR = foldMR . replicate

foldRepL :: Int -> [a] -> [a]
foldRepL = foldML . replicate

test :: IO ()
test = do
  let
    list = [10, 20, 30] :: [Integer]
    title x y = "FAILED: " <> show x <> " should be equal to " <> show y
    tests =
      TestList
        [ TestCase
            ( let f = foldr (+)
                  actual = f 1 list
                  expect = 61
               in assertEqual (title expect actual) expect actual
            )
        , TestCase
            ( let f = foldr (*)
                  actual = f 1 list
                  expect = 6000
               in assertEqual (title expect actual) expect actual
            )
        , TestCase
            ( let f = foldRepR
                  actual = f 2 list
                  expect = [10, 10, 20, 20, 30, 30]
               in assertEqual (title expect actual) expect actual
            )
        , TestCase
            ( let f = foldRepL
                  actual = f 2 list
                  expect = [10, 10, 20, 20, 30, 30]
               in assertEqual (title expect actual) expect actual
            )
        ]
  _ <- runTestTT tests
  pure ()
