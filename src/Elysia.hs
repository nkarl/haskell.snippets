module Elysia where

import Prelude

import Text.Pretty.Simple

data Where = Here | There deriving (Show)
data Output = CO_2 | O_2 deriving (Show)

class Livable where
  breath :: Output
instance Livable where
  breath = CO_2

class Movable where
  move :: Where -> Where -> (Where, Where)
instance Movable where
  move x y = (x, y)

class Photosynthesizable where
  photosynth :: Output
instance Photosynthesizable where
  photosynth = O_2

data Elysia
  = Elysia
  { it_breathes_out :: Output
  , it_moves_from_to :: (Where, Where)
  , it_photosynthes_out :: Output
  }
  deriving (Show)

test :: IO ()
test = do
  let
    elysia =
      Elysia
        { it_breathes_out = breath
        , it_moves_from_to = move Here There
        , it_photosynthes_out = photosynth
        }
  pPrint elysia
