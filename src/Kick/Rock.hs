module Kick.Rock where

import Text.Pretty.Simple
import Prelude hiding (Right)

data Where = Here | There deriving (Show)
data Which = Rock | Dirt deriving (Show)
data Kick = Don't | Cry | Right Where deriving (Show)

data Something = Something
  { which_is :: Which
  , where_is :: Where
  }
  deriving (Show)

class Kickable where
  kick :: Maybe Something -> Maybe Kick

instance Kickable where
  kick = \something ->
    case something of
      Nothing -> Just Cry
      Just (Something{which_is, where_is}) -> case where_is of
        There -> Nothing
        Here -> case which_is of
          Rock -> Just $ Right There
          Dirt -> Just Don't

data Result = Result
  { it_is :: Maybe Something
  , kicked :: Maybe Kick
  }
  deriving (Show)

test :: IO ()
test = do
  let
    thing1 = Just $ Something{which_is = Rock, where_is = Here}
    thing2 = Just $ Something{which_is = Rock, where_is = There}
    thing3 = Nothing
  pPrint $ Result{it_is = thing1, kicked = kick thing1}
  pPrint $ Result{it_is = thing2, kicked = kick thing2}
  pPrint $ Result{it_is = thing3, kicked = kick thing3}
