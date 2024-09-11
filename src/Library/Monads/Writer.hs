module Library.Monads.Writer where

import Control.Monad (ap)
import Prelude

newtype Writer s a
  = Writer (a, s)

instance Functor (Writer s) where
  fmap f (Writer (x, l)) = Writer (f x, l)

instance (Monoid s) => Applicative (Writer s) where
  (<*>) (Writer (f, s1)) (Writer (x, s2)) = Writer (f x, s1 <> s2)
  pure x = Writer (x, mempty)

instance (Monoid s) => Monad (Writer s) where
  (>>=) (Writer (x, s1)) f = do
    let (Writer (y, s2)) = f x
    Writer (y, s1 <> s2)

tell :: forall s. s -> Writer s ()
tell s = Writer ((), s)

listen :: forall a s. Writer s a -> Writer s (a, s)
listen (Writer t) = do
  let (_, s) = t
  Writer (t, s)

pass :: forall a s. Writer s (a, s -> s) -> Writer s a
pass (Writer t) = do
  let
    (t', s) = t
    (value, f) = t'
  Writer (value, f s)

test :: IO ()
test = do
  putStrLn "placeholder"
