module Library.Test where

import Library.Foldable

runTests :: IO ()
runTests = do
  _ <- Library.Foldable.test
  pure ()
