module Library.Traversable where

import Prelude hiding (Traversable, traverse, sequenceA)

class (Functor t, Foldable t) => Traversable t where
  traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
  sequenceA :: forall a m. (Applicative m) => t (m a) -> m (t a)

test :: IO ()
test = do
  print "placeholder"
