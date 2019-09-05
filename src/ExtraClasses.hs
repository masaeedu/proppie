module ExtraClasses where

class (forall m. Monad m => Monad (t m)) => MFunctor t where
  mmap :: (Monad m, Monad n) => (forall a. m a -> n a) -> t m a -> t n a

class MFunctor t => MMonad t where
  mpure :: Monad m => m a -> t m a
  mbind :: (Monad m, Monad n) => (forall a. m a -> t n a) -> t m a -> t n a
