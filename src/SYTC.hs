{-# LANGUAGE TupleSections #-}

module SYTC where

import Prelude as P
import Data.Tuple
import Data.Either.Combinators

import Types

right_from_left dimap left = dimap swapEither swapEither . left
second_from_first dimap first = dimap swap swap . first
unsecond_from_unfirst dimap unfirst = unfirst . dimap swap swap

upstream_dimap :: Functor m => (x' -> x) -> (y -> y') -> Proxy b' y x b m v  -> Proxy b' y' x' b m v
upstream_dimap f g = rec
  where
  rec (Request a' r) = Request (g a') $ rec . r . f
  rec (Respond b  r) = Respond b $ rec . r
  rec (M mr)         = M $ rec <$> mr
  rec (Pure v)       = Pure v

upstream_left :: (Monoid v, Functor m) => Proxy b' a' a b m v -> Proxy b' (Either a' x) (Either a x) b m v
upstream_left = rec
  where
  rec (Request a' r) = Request (Left a') $ either (rec . r) (const $ Pure P.mempty)
  rec (Respond b  r) = Respond b $ rec . r
  rec (M mr)         = M $ rec <$> mr
  rec (Pure v)       = Pure v

upstream_right :: (Monoid v, Functor m) => Proxy b' a' a b m v -> Proxy b' (Either x a') (Either x a) b m v
upstream_right = right_from_left upstream_dimap upstream_left

upstream_unfirst :: Functor m => Proxy b' (a', x) (a, x) b m v -> Proxy b' a' a b m v
upstream_unfirst = rec
  where
  rec (Request (a', x) r) = Request a' $ rec . r . (, x)
  rec (Respond b       r) = Respond b $ rec . r
  rec (M mr)              = M $ rec <$> mr
  rec (Pure v)            = Pure v

upstream_unsecond :: Functor m => Proxy b' (x, a') (x, a) b m v -> Proxy b' a' a b m v
upstream_unsecond = unsecond_from_unfirst upstream_dimap upstream_unfirst

downstream_dimap :: Functor m => (x' -> x) -> (y -> y') -> Proxy x a' a y m v -> Proxy x' a' a y' m v
downstream_dimap f g = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond b  r) = Respond (g b) $ rec . r . f
  rec (M mr)         = M $ rec <$> mr
  rec (Pure v)       = Pure v

downstream_left :: (Monoid v, Functor m) => Proxy b' a' a b m v -> Proxy (Either b' x) a' a (Either b x) m v
downstream_left = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond b  r) = Respond (Left b) $ either (rec . r) (const $ Pure P.mempty)
  rec (M mr)         = M $ rec <$> mr
  rec (Pure v)       = Pure v

downstream_right :: (Monoid v, Functor m) => Proxy b' a' a b m v -> Proxy (Either x b') a' a (Either x b) m v
downstream_right = right_from_left downstream_dimap downstream_left

downstream_unfirst :: Functor m => Proxy (b', x) a' a (b, x) m v -> Proxy b' a' a b m v
downstream_unfirst = rec
  where
  rec (Request a'     r) = Request a' $ rec . r
  rec (Respond (b, x) r) = Respond b $ rec . r . (, x)
  rec (M mr)             = M $ rec <$> mr
  rec (Pure v)           = Pure v

downstream_unsecond :: Functor m => Proxy (x, b') a' a (x, b) m v -> Proxy b' a' a b m v
downstream_unsecond = unsecond_from_unfirst downstream_dimap downstream_unfirst

fmap :: Functor m => (x -> y) -> Proxy b' a' a b m x -> Proxy b' a' a b m y
fmap f = rec
  where
  rec (Request a' x) = Request a' $ rec . x
  rec (Respond b  x) = Respond b $ rec . x
  rec (M mr)         = M (rec <$> mr)
  rec (Pure x)       = Pure $ f x

pure :: x -> Proxy b' a' a b m x
pure = Pure

liftA2 :: Functor m => (x -> y -> z) -> Proxy b' a' a b m x -> Proxy b' a' a b m y -> Proxy b' a' a b m z
liftA2 f = rec
  where
  rec (Request a' r) y = Request a' $ flip rec y . r
  rec (Respond b  r) y = Respond b $ flip rec y . r
  rec (M mr)         y = M $ flip rec y <$> mr
  rec (Pure x)       y = SYTC.fmap (f x) y

bind :: Functor m => (x -> Proxy b' a' a b m y) -> Proxy b' a' a b m x -> Proxy b' a' a b m y
bind amb = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond b  r) = Respond b $ rec . r
  rec (M mr)         = M $ rec <$> mr
  rec (Pure x)       = amb x

join :: Functor m => Proxy b' a' a b m (Proxy b' a' a b m v) -> Proxy b' a' a b m v
join = bind id

mmap :: Functor m => (forall a. m a -> n a) -> Proxy b' a' a b m v -> Proxy b' a' a b n v
mmap f = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond b  r) = Respond b $ rec . r
  rec (M mr)         = M $ f $ rec <$> mr
  rec (Pure x)       = Pure x

mpure :: Monad m => m v -> Proxy b' a' a b m v
mpure ma = M $ Pure <$> ma

mbind :: (Monad m, Monad n) => (forall v. m v -> Proxy b' a' a b n v) -> Proxy b' a' a b m v -> Proxy b' a' a b n v
mbind amb = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond b  r) = Respond b $ rec . r
  rec (M mr)         = join $ amb $ rec <$> mr
  rec (Pure x)       = Pure x

(<>) :: (Functor m, Semigroup v) => Proxy b' a' a b m v -> Proxy b' a' a b m v -> Proxy b' a' a b m v
(<>) = liftA2 (P.<>)

mempty :: (Functor m, Monoid v) => Proxy b' a' a b m v
mempty = Pure P.mempty
