{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StarIsType #-}

module Computation.Chain where

import Control.Applicative ((<|>))
import GHC.Base (List)
import Test.HUnit
import Prelude

import Text.Pretty.Simple (pPrint)

{--
 - NOTE: TECHNICAL HURDLES
 - 1. How to make a DAG in Haskell?
 -  - [ ] learn how to implement a tree (binary tree) first
 - 2. How should the forward and reverse actions be done?
--}

data Unary = Id | Neg deriving (Show)
data Binary = Add | Mul deriving (Show)
data Op = Unary Unary | Binary Binary deriving (Show)

data Scalar
  = Scalar
  { f :: Float
  , f' :: Float
  , op :: Maybe Op
  , input :: List Scalar
  }
  deriving (Show)

class QueryScalar where
  getF :: Scalar -> Float
  getF' :: Scalar -> Float
  getOp :: Scalar -> Maybe Op
  getInput :: Scalar -> List Scalar

instance QueryScalar where
  getF Scalar{f} = f
  getF' Scalar{f'} = f'
  getOp Scalar{op} = op <|> Nothing
  getInput Scalar{input} = input

class MathScalar where
  idScalar :: Scalar -> Scalar
  addScalar :: Scalar -> Scalar -> Float

instance MathScalar where
  idScalar = id
  addScalar Scalar{f = f1} Scalar{f = f2} = f1 + f2

test :: IO ()
test = do
  let
    s1 = Scalar{f = 1, f' = 0, op = Nothing, input = []}
    s2 = Scalar{f = 2, f' = 0, op = Nothing, input = []}
    s3 =
      Scalar
        { f = addScalar s1 s2
        , f' = 0
        , op = Just $ Binary Add
        , input = [s1, s2]
        }
  pPrint s3
