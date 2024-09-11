module Library.MooreMachine.Unit where

import Prelude

import Data.Profunctor

type Output s b = s -> b
type Step   s a = s -> a -> s

data FSM  s a b = FSM s (Output s b) (Step s a)

instance Profunctor (FSM s) where
  -- dimap :: forall a b c d. (a -> c) -> (b -> d) -> FSM s c b -> FSM s a d
  dimap f g (FSM s0 output step) = FSM s0 (g . output) (\s -> step s . f)

-- FSM s c b
-- FSM s a d

-- output           :: s -> b
-- g                :: b -> d
-- g . output       :: s -> d

-- step             :: s -> c -> s
-- f                :: a -> c
-- step s           :: c -> s
-- step s . f       :: a -> s
-- \s -> step s . f :: s -> (a -> s)

-- state S
data Oven = Off | Bake | Idling

-- input \Sigma
data Signal = ButtonBake | ButtonOff | TooHot | TooCold

-- output \Lambda
data Heat = Hot | Cold

-- checks a subset of the Cartesian product of S and \Sigma
stepFn :: Oven -> Signal -> Oven
stepFn x y = go (x, y)
 where
  go t = case t of
    (Off, ButtonBake) -> Bake
    (Bake, ButtonOff) -> Off
    (Bake, TooHot) -> Idling
    (Idling, TooCold) -> Bake
    (Idling, ButtonOff) -> Off
    (s, _) -> s

outputFn :: Oven -> Heat
outputFn t = case t of
  Off -> Cold
  Bake -> Hot
  Idling -> Cold

test :: IO ()
test = do
  print "placeholder"
