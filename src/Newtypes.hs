module Newtypes where

import Data.Profunctor
import Data.Profunctor.Choice
import Data.Profunctor.Strong
import Control.Applicative

import Data.Coerce

import Types
import ExtraClasses
import SYTC

newtype Upstream m v b' b a a'
  = Upstream (Proxy b' a' a b m v)

instance Functor m => Profunctor (Upstream m v b' b)
  where
  dimap = coerce upstream_dimap

instance (Monoid v, Functor m) => Choice (Upstream m v b' b)
  where
  left' = coerce upstream_left

instance Functor m => Costrong (Upstream m v b' b)
  where
  unfirst = coerce upstream_unfirst

newtype Downstream m v a a' b' b
  = Downstream (Proxy b' a' a b m v)

instance Functor m => Profunctor (Downstream m v a a')
  where
  dimap = coerce downstream_dimap

instance (Monoid v, Functor m) => Choice (Downstream m v a a')
  where
  left' = coerce downstream_left

instance Functor m => Costrong (Downstream m v a a')
  where
  unfirst = coerce downstream_unfirst

instance Functor m => Functor (Proxy b' a' a b m)
  where
  fmap = coerce SYTC.fmap

instance Functor m => Applicative (Proxy b' a' a b m)
  where
  pure = coerce SYTC.pure
  liftA2 = coerce SYTC.liftA2

instance Functor m => Monad (Proxy b' a' a b m)
  where
  (>>=) = flip $ coerce SYTC.bind

instance MFunctor (Proxy b' a' a b)
  where
  mmap = coerce SYTC.mmap

instance MMonad (Proxy b' a' a b)
  where
  mpure = coerce SYTC.mpure
  mbind = coerce SYTC.mbind

instance (Functor m, Semigroup v) => Semigroup (Proxy b' a' a b m v)
  where
  (<>) = (SYTC.<>)

instance (Functor m, Monoid v) => Monoid (Proxy b' a' a b m v)
  where
  mempty = SYTC.mempty
