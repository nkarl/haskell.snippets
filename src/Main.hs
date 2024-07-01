module Main where

import Prelude

-- import Haskell99.Test
-- import Computation.Chain
-- import MooreMachine.Unit
--import Sandbox.Trees.Binary
import Library.Test

main :: IO ()
main = do
  -- Haskell99.Test.test
  -- Computation.Chain.test
  -- MooreMachine.Unit.test
  --Sandbox.Trees.Binary.test
  Library.Test.runTests
