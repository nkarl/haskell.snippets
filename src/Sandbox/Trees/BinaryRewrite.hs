{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module Sandbox.Trees.BinaryRewrite (
  Tree (..),
  hasNode,
  insertNode,
  hasHeight,
  flatten,
  test,
)
where

import System.Random (randomRIO)
import Prelude

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Eq)

hasNode :: forall a. (Ord a) => a -> Tree a -> Bool
hasNode _ Empty = False
hasNode a (Node key left right)
  | key == a  = True
  | key > a   = hasNode a left
  | otherwise = hasNode a right

insertNode :: forall a. (Ord a) => a -> Tree a -> Tree a
insertNode a Empty = Node a Empty Empty
insertNode a (Node key left right)
  | key == a  = Node key left right
  | key > a   = Node key (insertNode a left) right
  | otherwise = Node key left (insertNode a right)

hasHeight :: forall a. Tree a -> Int
hasHeight = measure 0
 where
  measure :: Int -> Tree a -> Int
  measure h Empty = h
  measure h (Node _ left right) = maximum [h, h_left, h_right]
   where
    h_left  = measure (h + 1) left
    h_right = measure (h + 1) right

type MarginLeft = Int
type Thickness  = Int

instance (Show a) => Show (Tree a) where
  show Empty = ""
  show tree =
    let
      m = 0
      g = 1
      t = widestNum tree + g
     in
      makeShowable m t tree
   where
    makeShowable :: (Show a) => MarginLeft -> Thickness -> Tree a -> String
    makeShowable _ _ Empty = ""
    makeShowable x t (Node key left right) =
      let
        offset_right  = makeShowable (x + t) t right
        offset_root   = replicate x ' ' <> show key -- NOTE offset by `x` white-space from the left edge of screen
        offset_left   = makeShowable (x + t) t left
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

flatten     :: forall a. Tree a -> [a]
flatten     Empty = []
flatten     (Node a Empty Empty)  = [a]
flatten     (Node a left right)   = flatten left <> [a] <> flatten right

flattenPre  :: forall a. Tree a -> [a]
flattenPre  Empty = []
flattenPre  (Node a Empty Empty)  = [a]
flattenPre  (Node a left right)   = [a] <> flattenPre left <> flattenPre right

flattenPost :: forall a. Tree a -> [a]
flattenPost Empty = []
flattenPost (Node a Empty Empty)  = [a]
flattenPost (Node a left right)   = flattenPost left <> flattenPost right <> [a]

grow :: forall a. (Ord a) => [a] -> Tree a
grow = (foldr insertNode Empty) . reverse

randomTree :: [Int] -> IO (Tree Int)
randomTree nodes = do
  pure (grow nodes)

randomList :: Int -> IO [Int]
randomList 0 = pure []
randomList n
  | n > 1000  = randomList 1000
  | otherwise = do
      x   <- randomRIO (1, n)
      xs  <- randomList (n - 1)
      pure (x : xs)

-- TODO: implement a lazy generator for the random number list

test :: IO ()
test = do
  nodes <- randomList 16 
  tree  <- randomTree nodes
  print nodes >> print tree
  putStr " pre-order: " >> print (flattenPre  tree)
  putStr "  in-order: " >> print (flatten     tree)
  putStr "post-order: " >> print (flattenPost tree)
