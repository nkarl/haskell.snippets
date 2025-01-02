module Main where

import Prelude hiding (Just, Maybe, Nothing)

-- import Haskell99.Test
-- import Sandbox.Computation.Chain

import Sandbox.FileSystem.IOActions
import Sandbox.Trees.Binary
--import Sandbox.Trees.BinaryRewrite
import Sandbox.Trees.BinaryMaybe

import Library.MooreMachine.Unit
import Library.Test

main :: IO ()
main = do
  -- Haskell99.Test.test
  -- Computation.Chain.test
  -- MooreMachine.Unit.test
  --Sandbox.Trees.Binary.test
  --Sandbox.Trees.BinaryRewrite.test
  Sandbox.Trees.BinaryMaybe.test

-- Library.Test.runTests
-- MooreMachine.Unit.test
-- Sandbox.FileSystem.IOActions.test
