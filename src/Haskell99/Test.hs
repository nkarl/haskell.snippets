module Haskell99.Test where

import Prelude

import Text.Pretty.Simple (pPrint)

import Haskell99.P01.FindLast
import Haskell99.P02.FindButLast
import Haskell99.P03.FindkElement
import Haskell99.P04.ListLength

test :: IO ()
test = do
  pPrint "P01.FindLast: Integer in List of Integers"
  _ <- Haskell99.P01.FindLast.test
  pPrint "P02.FindButLast: Next-to-Last Integer in List of Integers"
  _ <- Haskell99.P02.FindButLast.test
  pPrint "P03.FindkElement: Integer at Index `k` in List of Integers"
  _ <- Haskell99.P03.FindkElement.test
  pPrint "P04.ListLength: Integer Lengths of Lists of Integers and Strings"
  _ <- Haskell99.P04.ListLength.test
  pure ()
