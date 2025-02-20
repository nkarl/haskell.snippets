module Main where

import Prelude hiding (Just, Maybe, Nothing)

-- import Haskell99.Test
-- import Sandbox.Computation.Chain

-- import Sandbox.FileSystem.IOActions
-- import qualified Sandbox.Trees.Binary as Binary
-- import qualified Sandbox.Trees.BinaryRewrite as BinaryRewrite
import Sandbox.Trees.BinaryRewrite02 qualified as BinaryRewrite02

-- import qualified Sandbox.Trees.BinaryMaybe as BinaryMaybe

-- import Library.MooreMachine.Unit
-- import Library.Test
import Library.Monads.State

main :: IO ()
main = do
    -- Haskell99.Test.test
    -- Computation.Chain.test
    -- MooreMachine.Unit.test
    -- Binary.test
    -- BinaryRewrite.test
    -- BinaryRewrite02.test
    -- BinaryMaybe.test

    -- Library.Test.runTests
    -- MooreMachine.Unit.test
    -- Sandbox.FileSystem.IOActions.test
    Library.Monads.State.demo
