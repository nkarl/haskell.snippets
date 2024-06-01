module Haskell99.Test where

import Prelude

import Text.Pretty.Simple (pPrint)

import Haskell99.P01.FindLast
import Haskell99.P02.FindButLast

test :: IO ()
test = do
  pPrint "P01.FindLast: Integer in List of Integers"
  _ <- Haskell99.P01.FindLast.testNum
  pPrint "P01.FindButLast: Next-to-Last Integer in List of Integers"
  _ <- Haskell99.P02.FindButLast.testNum
  pure ()
