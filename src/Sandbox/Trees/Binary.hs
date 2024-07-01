-- https://www.anardil.net/2018/binary-tree-in-haskell.html

module Sandbox.Trees.Binary where

import System.Random (randomRIO)
import Prelude

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Read, Eq)

-- | insert a node into the tree.
growNode :: (Ord a) => a -> Tree a -> Tree a
growNode x Empty = Node x Empty Empty
growNode x (Node root left right)
  | x == root = Node root left right
  | x < root  = Node root (growNode x left) right
  | x > root  = Node root left (growNode x right)
  | otherwise = Empty

-- | check if a value is a tree node.
isTreeNode :: (Ord a) => a -> Tree a -> Bool
isTreeNode _ Empty = False
isTreeNode x (Node root left right)
  | x == root = True
  | x < root  = isTreeNode x left
  | x > root  = isTreeNode x right
  | otherwise = False

-- | measure the tree's height.
treeHeight' :: Tree a -> Int -> Int
treeHeight' Empty h = h
treeHeight' (Node _ left right) h = maximum [h, lb, rb]
 where
  lb = treeHeight' left (h + 1)
  rb = treeHeight' right (h + 1)

treeHeight :: Tree a -> Int
treeHeight = flip treeHeight' 0

instance (Show a) => Show (Tree a) where
  show Empty = ""
  show tree = show' tree 0 (widestElem tree + 1)

show' :: (Show a) => Tree a -> Int -> Int -> String
show' Empty _ _ = ""
show' (Node root left right) depth width = offset_r ++ "\n" ++ offset_c ++ offset_l
 where
  offset_c = replicate depth ' ' ++ show root
  offset_l = show' left (depth + width) width
  offset_r = show' right (depth + width) width

widestElem :: (Show a) => Tree a -> Int
widestElem Empty = 0
widestElem (Node root left right) = maximum [l, r, c]
 where
  l = widestElem left
  r = widestElem right
  c = length $ show root

makeTree :: (Ord a) => [a] -> Tree a
makeTree = foldr growNode Empty . reverse

randomTree :: Int -> IO (Tree Int)
randomTree n = do
  numbers <- randomList n
  return $ makeTree numbers

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n
  | n > 1_000 = randomList 1000
  | otherwise = do
      r <- randomRIO (1, n)
      rs <- randomList (n - 1)
      return (r : rs)

test :: IO ()
test = do
  print "placeholder"
