module Newtypes where

import Data.Profunctor
import Control.Applicative

import Data.Coerce

import Types
import ExtraClasses
import SYTC

instance Functor m => Profunctor (Proxy m v b' a')
  where
  dimap = downstream_dimap

newtype Proxy' m v a b b' a'
  = Proxy' (Proxy m v b' a' a b)

instance Functor m => Profunctor (Proxy' m v a b)
  where
  dimap = coerce upstream_dimap

newtype Proxy'' b' a' a b m v
  = Proxy'' (Proxy m v b' a' a b)

instance Functor m => Functor (Proxy'' b' a' a b m)
  where
  fmap = coerce SYTC.fmap

instance Functor m => Applicative (Proxy'' b' a' a b m)
  where
  pure = coerce SYTC.pure
  liftA2 = coerce SYTC.liftA2

instance Functor m => Monad (Proxy'' b' a' a b m)
  where
  (>>=) = flip $ coerce SYTC.bind

instance MFunctor (Proxy'' b' a' a b)
  where
  mmap = coerce SYTC.mmap

instance MMonad (Proxy'' b' a' a b)
  where
  mpure = coerce SYTC.mpure
  mbind = coerce SYTC.mbind
