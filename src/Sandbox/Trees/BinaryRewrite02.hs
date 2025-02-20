{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Sandbox.Trees.BinaryRewrite02 where

import Prelude

data Tree a
    = Nil
    | Node a (Tree a) (Tree a)
    deriving (Eq)

hasNode :: forall a. (Ord a) => a -> Tree a -> Bool
hasNode _ Nil = False
hasNode a (Node key left right)
    | a == key  = True
    | a <  key  = hasNode a left
    | otherwise = hasNode a right

insertNode :: forall a. (Ord a) => a -> Tree a -> Tree a
insertNode a Nil = Node a Nil Nil
insertNode a t@(Node key left right)
    | key == a  = t
    | key >  a  = Node key (insertNode a left) right
    | otherwise = Node key left (insertNode a right)

hasHeight :: forall a. Tree a -> Int
hasHeight = measure 0
  where
    measure :: Int -> Tree a -> Int
    measure h Nil = h
    measure h (Node _ left right) = maximum [h, h_left, h_right]
      where
        h_left = measure (h + 1) left
        h_right = measure (h + 1) right

flipTree :: forall a. Tree a -> Tree a
flipTree Nil = Nil
flipTree (Node a left right) = Node a (flipTree right) (flipTree left)

type MarginLeft = Int
type Thickness = Int

instance (Show a) => Show (Tree a) where
    show Nil = ""
    show tree =
        let
            m = 0
            g = 1
            t = widestNum tree + g
         in
            makeShowable m t tree
      where
        makeShowable :: (Show a) => MarginLeft -> Thickness -> Tree a -> String
        makeShowable _ _ Nil = ""
        makeShowable x t (Node key left right) =
            let
                offset_right = makeShowable (x + t) t right
                offset_root = replicate x ' ' <> show key -- NOTE offset by `x` white-space from the left edge of screen
                offset_left = makeShowable (x + t) t left
             in
                offset_right <> "\n" <> offset_root <> offset_left

        -- find the node whose key has the widest string representation.
        -- This gap is the minimum space required between tree layers for a consistent visual.
        widestNum :: (Show a) => Tree a -> Int
        widestNum Nil = 0
        widestNum (Node key left right) =
            let
                r = widestNum right
                c = length $ show key
                l = widestNum left
             in
                maximum [r, c, l]

makeTree :: forall a. (Ord a) => [a] -> Tree a
makeTree = (foldr insertNode Nil) . reverse

flatten :: forall a. Tree a -> [a]
flatten Nil                     = []
flatten (Node a Nil Nil)        = [a]
flatten (Node a left right)     = flatten left <> [a] <> flatten right

flattenPre :: forall a. Tree a -> [a]
flattenPre Nil                  = []
flattenPre (Node a Nil Nil)     = [a]
flattenPre (Node a left right)  = [a] <> flatten left <> flatten right

flattenPost :: forall a. Tree a -> [a]
flattenPost Nil                 = []
flattenPost (Node a Nil Nil)    = [a]
flattenPost (Node a left right) = flatten left <> flatten right <> [a]

test :: IO ()
test = do
    let
        nums = [6, 4, 3, 5, 13, 1, 9, 25] :: [Int]
        tree = makeTree nums
    print tree
    print $ "tree hasHeight: " <> show (hasHeight tree)
    print $ flatten tree
    print $ flattenPre tree
    print $ flattenPost tree
    print $ flipTree tree
