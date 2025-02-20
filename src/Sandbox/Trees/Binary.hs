{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Sandbox.Trees.Binary where

import System.Random (randomRIO)
import Prelude

{- |
Reference: https://www.anardil.net/2018/binary-tree-in-haskell.html

A data type called `Tree` with 2 data constructors. The second constructor takes 3 type params,
2 of which are recursive data constructors.

NOTE: rule of thumb for every data type,
    - every data variant must be accounted for, and
    - all recursive constructors must be accounted for.
-}
data Tree a
    = Empty
    | Node a (Tree a) (Tree a)
    deriving (Eq)

{-
   TODO: think about how to implement a class of Functor, Applicative and Monad for a Binary Tree.
-}

-- | check if a value exists inside the tree.
hasNode :: forall a. (Ord a) => a -> Tree a -> Bool
hasNode _ Empty = False
hasNode a (Node key leftTree rightTree)
    | key == a = True
    | key < a = hasNode a rightTree
    | key > a = hasNode a leftTree
    | otherwise = False

-- | insert a node into the tree. the tree is always ordered.
insertNode :: forall a. (Ord a) => a -> Tree a -> Tree a
insertNode a Empty = Node a Empty Empty
insertNode a (Node key leftTree rightTree)
    | key == a = Node key leftTree rightTree
    | key < a = Node key leftTree (insertNode a rightTree)
    | key > a = Node key (insertNode a leftTree) rightTree
    | otherwise = Empty

-- | measure the tree's hasHeight.
hasHeight :: forall a. Tree a -> Int
hasHeight = measure 0
  where
    measure :: Int -> Tree a -> Int
    measure h Empty = h
    measure h (Node _ leftTree rightTree) = maximum [h, rt, lt]
      where
        rt = measure (h + 1) rightTree
        lt = measure (h + 1) leftTree

type MarginLeft = Int
type Thickness = Int

instance (Show a) => Show (Tree a) where
    show Empty = ""
    show tree =
        let
            margin = 0; gap = 1; thickness = findMax tree + gap
         in
            makeShowable margin thickness tree
      where
        makeShowable :: (Show a) => MarginLeft -> Thickness -> Tree a -> String
        makeShowable _ _ Empty = ""
        makeShowable x t (Node key leftTree rightTree) =
            let
                offset_r = makeShowable (x + t) t rightTree
                offset_c = replicate x ' ' <> show key -- NOTE offset by `x` white-space from the leftTree margin
                offset_l = makeShowable (x + t) t leftTree
             in
                offset_r <> "\n" <> offset_c <> offset_l

        -- find the maximum node; its key should also have the widest string representation.
        -- This is the minimum thickness for each tree later for a consistant visual.
        findMax :: (Show a) => Tree a -> Int
        findMax Empty = 0
        findMax (Node key leftTree rightTree) =
            let
                r = findMax rightTree
                c = length $ show key
                l = findMax leftTree
             in
                maximum [r, c, l]

makeTreeFromList :: forall a. (Ord a) => [a] -> Tree a
makeTreeFromList = (foldl (flip insertNode) Empty)

makeRandomTree :: Int -> IO (Tree Int)
makeRandomTree n = do
    numbers <- randomList n
    pure $ makeTreeFromList numbers

-- generates a list of `n` random integers.
randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n
    | n > 1_000 = randomList 1_000
    | otherwise = do
        x <- randomRIO (1, n)
        xs <- randomList (n - 1)
        pure (x : xs)

test :: IO ()
test = do
    -- nums <- randomList 10
    let
        nums = [6, 4, 3, 5, 13, 1, 9, 25] :: [Int]
        tree = foldr insertNode (makeTreeFromList nums) [32, 7, 17]
    print $ "tree hasHeight: " <> show (hasHeight tree)
    print tree
