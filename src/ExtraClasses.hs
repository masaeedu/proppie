module ExtraClasses where

import Data.Profunctor
import Control.Applicative

class (forall m. Monad m => Monad (t m)) => MFunctor t where
  mmap :: (Monad m, Monad n) => (forall a. m a -> n a) -> t m a -> t n a

class MFunctor t => MMonad t where
  mpure :: Monad m => m a -> t m a
  mbind :: (Monad m, Monad n) => (forall a. m a -> t n a) -> t m a -> t n a

class Profunctor p => Semigroupal p where
  zip :: p a b -> p c d -> p (a, c) (b, d)

class Semigroupal p => Monoidal p where
  infinite :: p () ()

instance Applicative f => Semigroupal (Star f)
  where
  zip (Star afb) (Star cfd) = Star $ \(a, c) -> liftA2 (,) (afb a) (cfd c)
