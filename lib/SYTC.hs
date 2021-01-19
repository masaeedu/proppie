{-# LANGUAGE LambdaCase, TupleSections #-}
module SYTC where

import Prelude as P
import Control.Applicative as A
import Data.Tuple
import Data.Either.Combinators
import Control.Monad (forever, join)
import qualified Data.Bifunctor as B
import qualified Control.Category as C

import Pipes

fmap_from_dimap dimap = dimap id
right_from_left dimap left = dimap swapEither swapEither . left
second_from_first dimap first = dimap swap swap . first
unsecond_from_unfirst dimap unfirst = unfirst . dimap swap swap

upstream_dimap :: Functor m => (x' -> x) -> (y -> y') -> Proxy y x b' b m v  -> Proxy y' x' b' b m v
upstream_dimap f g = rec
  where
  rec (Request a' r) = Request (g a') $ rec . r . f
  rec (Respond r)    = Respond ((rec <$>) . r)
  rec (M mr)         = M $ rec <$> mr
  rec (Pure v)       = Pure v

upstream_fmap :: Functor m => (x -> x') -> Proxy x a b' b m v -> Proxy x' a b' b m v
upstream_fmap = fmap_from_dimap upstream_dimap

upstream_pure :: Functor m => x -> Proxy x a b' b m v
upstream_pure x = forever $ request x

upstream_liftA2 :: Monad m => (x -> y -> z) -> Proxy x a b' b m v -> Proxy y a b' b m v -> Proxy z a b' b m v
upstream_liftA2 f = rec
  where
  -- When the first pipe is sitting on a request,
  -- take the request and combine it with the next
  -- request in the second pipe (dropping it if the
  -- other pipe is exhausted)
  rec    (Request x r1) (Request y r2) = Request (f x y) $ A.liftA2 rec r1 r2
  rec r1@(Request _ _)  (Respond r2)   = Respond $ (rec r1 <$>) . r2
  rec r1@(Request _ _)  (M mr)         = M $ rec r1 <$> mr
  rec    (Request _ _)  (Pure v)       = Pure v

  -- Skip everything but requests in the first pipe
  rec    (Respond r)    y              = Respond $ (flip rec y <$>) . r
  rec    (M mr)         y              = M $ flip rec y <$> mr
  rec    (Pure v)       _              = Pure v

upstream_left :: (Monoid v, Functor m) => Proxy a' a b' b m v -> Proxy (Either a' x) (Either a x) b' b m v
upstream_left = rec
  where
  rec (Request a' r) = Request (Left a') $ either (rec . r) (const $ Pure P.mempty)
  rec (Respond r)    = Respond $ (rec <$>) . r
  rec (M mr)         = M $ rec <$> mr
  rec (Pure v)       = Pure v

upstream_right :: (Monoid v, Functor m) => Proxy a' a b' b m v -> Proxy (Either x a') (Either x a) b' b m v
upstream_right = right_from_left upstream_dimap upstream_left

upstream_unfirst :: Functor m => Proxy (a', x) (a, x) b' b m v -> Proxy a' a b' b m v
upstream_unfirst = rec
  where
  rec (Request (a', x) r) = Request a' $ rec . r . (, x)
  rec (Respond r)         = Respond $ (rec <$>) . r
  rec (M mr)              = M $ rec <$> mr
  rec (Pure v)            = Pure v

upstream_unsecond :: Functor m => Proxy (x, a') (x, a) b' b m v -> Proxy a' a b' b m v
upstream_unsecond = unsecond_from_unfirst upstream_dimap upstream_unfirst

downstream_dimap :: Functor m => (x' -> x) -> (y -> y') -> Proxy a' a x y m v -> Proxy a' a x' y' m v
downstream_dimap f g = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond r)    = Respond $ B.bimap g rec . r . f
  rec (M mr)         = M $ rec <$> mr
  rec (Pure v)       = Pure v

downstream_fmap :: Functor m => (x -> y) -> Proxy a' a b' x m v -> Proxy a' a b' y m v
downstream_fmap = fmap_from_dimap downstream_dimap

downstream_left :: (Monoid v, Functor m) => Proxy a' a b' b m v -> Proxy a' a (Either b' x) (Either b x) m v
downstream_left = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond r)    = Respond $ either (B.bimap Left rec . r) ((, mempty) . Right)
  rec (M mr)         = M $ rec <$> mr
  rec (Pure v)       = Pure v

downstream_right :: (Monoid v, Functor m) => Proxy a' a b' b m v -> Proxy a' a (Either x b') (Either x b) m v
downstream_right = right_from_left downstream_dimap downstream_left

downstream_first :: Functor m => Proxy a' a b' b m v -> Proxy a' a (b', x) (b, x) m v
downstream_first = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond r)    = Respond $ \(b', x) -> B.bimap (, x) rec $ r b'
  rec (M mr)         = M $ rec <$> mr
  rec (Pure v)       = Pure v

downstream_second :: Functor m => Proxy a' a b' b m v -> Proxy a' a (x, b') (x, b) m v
downstream_second = second_from_first downstream_dimap downstream_first

downstream_zip :: Functor m => Proxy a' a b1' b1 m v -> Proxy a' a b2' b2 m v -> Proxy a' a (b1', b2') (b1, b2) m v
downstream_zip = rec
  where
  rec    (Respond r1)   (Respond r2)    = Respond $ B.bimap r1 r2 C.>>> \case ((b1, r1'), (b2, r2')) -> ((b1, b2), rec r1' r2')
  rec r1@(Respond _)    (Request a' r2) = Request a' $ rec r1 . r2
  rec r1@(Respond _)    (M mr)          = M $ rec r1 <$> mr
  rec    _              (Pure v)        = Pure v

  rec    (Request a' r) y               = Request a' $ flip rec y . r
  rec    (M mr)         y               = M $ flip rec y <$> mr
  rec    (Pure v)       _               = Pure v

mmap :: Functor m => (forall a. m a -> n a) -> Proxy b' a' a b m v -> Proxy b' a' a b n v
mmap f = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond r)    = Respond $ (rec <$>) . r
  rec (M mr)         = M $ f $ rec <$> mr
  rec (Pure x)       = Pure x

mpure :: Monad m => m v -> Proxy b' a' a b m v
mpure ma = M $ Pure <$> ma

mbind :: (Monad m, Monad n) => (forall v. m v -> Proxy b' a' a b n v) -> Proxy b' a' a b m v -> Proxy b' a' a b n v
mbind amb = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond r)    = Respond $ (rec <$>) . r
  rec (M mr)         = join $ amb $ rec <$> mr
  rec (Pure x)       = Pure x
