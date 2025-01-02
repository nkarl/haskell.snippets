module Sandbox.Trees.BinaryMaybe where

import Data.Maybe (catMaybes)
import Prelude

data Tree a
  = Node a (Maybe (Tree a)) (Maybe (Tree a))
  deriving (Show, Eq)

hasNode :: forall a. (Ord a) => a -> Maybe (Tree a) -> Bool
hasNode _ Nothing = False
hasNode a (Just (Node key left right))
  | key == a  = True
  | key < a   = go (Just left)
  | otherwise = go (Just right)
 where
  go t = case t of
    Nothing -> False
    Just y -> hasNode a y

insertNode :: forall a. (Ord a) => a -> Maybe (Tree a) -> Maybe (Tree a)
insertNode a Nothing = Just $ Node a Nothing Nothing
insertNode a (Just (Node key left right))
  | key == a  = Just $ Node key  left      right
  | key <  a  = Just $ Node key (go left)  right
  | otherwise = Just $ Node key  left     (go right)
 where
  go t = case t of
    Nothing -> Just $ Node a Nothing Nothing
    _ -> insertNode a t

makeTree :: forall a. (Ord a) => [a] -> Maybe (Tree a)
makeTree [] = Nothing
makeTree xs =
  let
    root = Just $ Node (head xs) Nothing Nothing
   in
    foldr insertNode root (tail xs)

flatten :: Tree a -> [[a]]
flatten tree0 = go [tree0]
 where
  go [] = []
  go trees = key : go (descendants >>= catMaybes)
   where
    (key, descendants) = unzip $ collect <$> trees
    collect (Node x left right) = (x, [left, right])

-- flatten :: forall a. Maybe (Tree a) -> Maybe [a]
-- flatten Nothing = Nothing
-- flatten (Just (Node a Nothing Nothing)) = Just [a]
-- flatten (Just (Node a left right)) =
-- flatten left <> Just [a] <> flatten right

perfectTree :: Tree Int
perfectTree = go 0 where
    go !n = Node n (Just $ go (n * 2 + 1)) (Just $ go (n * 2 + 2))

test :: IO ()
test = do
  --let
    --nodes = [0 .. 9] :: [Int]
  --Just t <- pure $ makeTree nodes
  --print $ flatten t
  print . last . take 3 $ flatten perfectTree
