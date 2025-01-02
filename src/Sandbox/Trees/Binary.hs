-- https://www.anardil.net/2018/binary-tree-in-haskell.html
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Sandbox.Trees.Binary where

import System.Random (randomRIO)
import Prelude

{--
 - A data type called `Tree` with 2 data constructors,
 - the second takes 3 type params, 2 of which are recursive data constructors.

 - NOTE: rule of thumb: for every data type,
    - each of its cases must be accounted for, and
    - all recursive constructor calls must be accounted for.
--}
data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Eq)

-- | check if a value exists inside the tree.
hasNode :: forall a. (Ord a) => a -> Tree a -> Bool
hasNode _ Empty = False
hasNode a (Node key left right)
  | a == key  = True
  | a >  key  = hasNode a right
  | a <  key  = hasNode a left
  | otherwise = False

-- | insert a node into the tree. the tree is always ordered.
insertNode :: forall a. (Ord a) => a -> Tree a -> Tree a
insertNode a Empty = Node a Empty Empty
insertNode a (Node key left right)
  | a == key  = Node key left                 right
  | a <  key  = Node key (insertNode a left)  right
  | a >  key  = Node key left                 (insertNode a right)
  | otherwise = Empty

-- | measure the tree's hasHeight.
hasHeight :: forall a. Tree a -> Int
hasHeight = measure 0
 where
  measure :: Int -> Tree a -> Int
  measure h Empty = h
  measure h (Node _ left right) = maximum [h, rb, lb]
   where
    rb = measure (h + 1) right
    lb = measure (h + 1) left

type MarginLeft = Int
type Thickness  = Int

instance (Show a) => Show (Tree a) where
  show Empty = ""
  show tree =
    let
      margin    = 0
      gap       = 1
      thickness = widestNum tree + gap
     in
      makeShowable margin thickness tree
   where
    makeShowable :: (Show a) => MarginLeft -> Thickness -> Tree a -> String
    makeShowable _ _ Empty = ""
    makeShowable x t (Node key left right) =
      let
        offset_right = makeShowable (x + t) t right
        offset_root  = replicate x ' ' <> show key -- NOTE offset by `x` white-space from the left edge of screen
        offset_left  = makeShowable (x + t) t left
       in
        offset_right <> "\n" <> offset_root <> offset_left

    -- find the node whose key has the widest string representation.
    -- This gap is the minimum space required between tree layers for a consistent visual.
    widestNum :: (Show a) => Tree a -> Int
    widestNum Empty = 0
    widestNum (Node key left right) =
      let
        r = widestNum right
        c = length $ show key
        l = widestNum left
       in
        maximum [r, c, l]

makeTree :: forall a. (Ord a) => [a] -> Tree a
makeTree = (foldr insertNode Empty) . reverse

randomTree :: Int -> IO (Tree Int)
randomTree n = do
  numbers <- randomList n
  pure $ makeTree numbers

-- generates a list of `n` random integers.
randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n
  | n > 1_000 = randomList 1_000
  | otherwise = do
      r  <- randomRIO  (1, n)
      rs <- randomList (n - 1)
      pure (r : rs)

test :: IO ()
test = do
  --nums <- randomList 10
  let
    nums = [6, 4, 3, 5, 13, 1, 9, 25] :: [Int]
    tree = insertNode 7 $ insertNode 17 $ makeTree nums
  print $ "tree hasHeight: " <> show (hasHeight tree)
  print tree
