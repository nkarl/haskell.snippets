module Haskell99.Test where

import Prelude

import Haskell99.P01.FindLast
import Text.Pretty.Simple (pPrint)

test :: IO ()
test = do
  pPrint "P01.FindLast: Integer in List of Integers"
  _ <- Haskell99.P01.FindLast.testNum
  pure ()
