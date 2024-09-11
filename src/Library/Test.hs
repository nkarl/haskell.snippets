module Library.Test where

import Library.Foldable
import Library.Monads.Writer
import Library.Monads.Parser

runTests :: IO ()
runTests = do
  _ <- Library.Foldable.test
  _ <- Library.Monads.Writer.test
  _ <- Library.Monads.Parser.test
  pure ()
