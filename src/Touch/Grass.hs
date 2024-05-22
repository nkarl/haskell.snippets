module Touch.Grass where

import Text.Pretty.Simple
import Prelude hiding (Right)

data Where = Here | There deriving (Show)
data Which = Grass | Poop | Bug deriving (Show)
data Touch = Right Where | Don't | Might deriving (Show)

data Something = Something
  { which_is :: Which
  , where_is :: Where
  }
  deriving (Show)

class Touchable where
  touch :: Maybe Something -> Maybe Touch

instance Touchable where
  touch = \something -> do
    Something{which_is, where_is} <- something
    case where_is of
      There -> Nothing
      Here -> case which_is of
        Grass -> Just $ Right Here
        Poop -> Just Don't
        Bug -> Just Might

data Result = Result
  { it_is :: Maybe Something
  , touched :: Maybe Touch
  }
  deriving (Show)

test :: IO ()
test = do
  let
    thing1 = Just $ Something{which_is = Grass, where_is = Here}
    thing2 = Just $ Something{which_is = Grass, where_is = There}
    thing3 = Just $ Something{which_is = Poop, where_is = Here}
    thing4 = Just $ Something{which_is = Bug, where_is = Here}
    thing5 = Nothing
  pPrint $ Result{it_is = thing1, touched = touch thing1}
  pPrint $ Result{it_is = thing2, touched = touch thing2}
  pPrint $ Result{it_is = thing3, touched = touch thing3}
  pPrint $ Result{it_is = thing4, touched = touch thing4}
  pPrint $ Result{it_is = thing5, touched = touch thing5}
