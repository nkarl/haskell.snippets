{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Library.Maybe where

import Prelude hiding (Just, Maybe, Nothing, map, ) -- Functor, fmap, Applicative, Monad,)

data Maybe a = Nothing | Just a deriving (Show, Eq, Ord)

--class Functor' m where
  --map :: forall a b. (a -> b) -> m a -> m b

class Functor m => Apply' m where
  apply :: forall a b. m (a -> b) -> m a -> m b

class Apply' m => Applicative' m where
  unit :: forall a. a -> m a

class Applicative' m => Bind' m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class Applicative' m => Join' m where
  join :: forall a. m (m a) -> m a

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x

instance Apply' Maybe where
  apply Nothing _ = Nothing
  apply (Just f) x = fmap f x

instance Applicative' Maybe where
  unit = Just

instance Applicative Maybe where
  pure = unit
  (<*>) = apply

instance Bind' Maybe where
  bind Nothing _ = Nothing
  bind (Just x) f = f x

instance Join' Maybe where
  join = (flip bind) id

instance Monad Maybe where
  (>>=) = bind

infixl 4 <.>
(<.>) :: (Applicative m) => m a -> m (a -> b) -> m b
(<.>) = flip (<*>)

test :: IO ()
test = do
  let
    x           = Just 1 :: Maybe Int
    expected    = Just 7 :: Maybe Int
    compute     = (+ 2) . (+ 1)

    unitComputeCurried  = unit compute    -- this is partial application of a function
    unitComputeComposed = unit . compute  -- this is function composition

    joining     = join . fmap (unit . compute)
    joining'    = join . fmap unit . fmap compute -- functor law

  -- JOINING
  print $ (Just 4) == joining  x
  print $ (Just 4) == joining' x

  -- APPLYING MANY TIMES
  print $ expected == ( apply unitComputeCurried
                          ( apply unitComputeCurried x ) )
  print $ expected == ( unitComputeCurried
                          <*> ( unitComputeCurried
                                  <*> x ) )
  print $ expected == ( x
                          <.> unitComputeCurried
                          <.> unitComputeCurried )   -- NOTE: very similar to BINDING
  -- JOINING MANY TIMES
  print $ expected == ( join . fmap (unit . compute . compute) ) x
  -- BINDING MANY TIMES
  print $ expected == ( (unit . compute . compute) =<< x )
  print $ expected == ( x >>= (unit . compute . compute) )
  print $ expected == ( x
                          >>= unitComputeComposed
                          >>= unitComputeComposed ) -- NOTE: similar to flippedApply
  -- COMPUTING MANY TIMES INSIDE DO
  print $ expected == do
                        a <- x
                        let
                          b = compute a
                          c = compute b
                        unit c

{--
  NOTE: joining and joining' show associative composition
--}
