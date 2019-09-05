module SYTC where

import Types

downstream_dimap :: Functor m => (x' -> x) -> (y -> y') -> Proxy m v b' a' x y -> Proxy m v b' a' x' y'
downstream_dimap f g = rec
  where
  rec (Request a' r) = Request a'    $ rec . r . f
  rec (Respond b  r) = Respond (g b) $ rec . r
  rec (M mr)         = M (rec <$> mr)
  rec (Pure v)       = Pure v

upstream_dimap :: Functor m => (x' -> x) -> (y -> y') -> Proxy m v x y a b -> Proxy m v x' y' a b
upstream_dimap f g = rec
  where
  rec (Request a' r) = Request (g a') $ rec . r
  rec (Respond b  r) = Respond b $ rec . r . f
  rec (M mr)         = M (rec <$> mr)
  rec (Pure v)       = Pure v

fmap :: Functor m => (x -> y) -> Proxy m x b' a' a b -> Proxy m y b' a' a b
fmap f = rec
  where
  rec (Request a' x) = Request a' $ rec . x
  rec (Respond b  x) = Respond b $ rec . x
  rec (M mr)         = M (rec <$> mr)
  rec (Pure x)       = Pure $ f x

pure :: x -> Proxy m x b' a' a b
pure = Pure

liftA2 :: Functor m => (x -> y -> z) -> Proxy m x b' a' a b -> Proxy m y b' a' a b -> Proxy m z b' a' a b
liftA2 f = rec
  where
  rec (Request a' r) y = Request a' $ flip rec y . r
  rec (Respond b  r) y = Respond b $ flip rec y . r
  rec (M mr)         y = M $ flip rec y <$> mr
  rec (Pure x)       y = SYTC.fmap (f x) y

bind :: Functor m => (x -> Proxy m y b' a' a b) -> Proxy m x b' a' a b -> Proxy m y b' a' a b
bind amb = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond b  r) = Respond b $ rec . r
  rec (M mr)         = M $ rec <$> mr
  rec (Pure x)       = amb x

join :: Functor m => Proxy m (Proxy m v b' a' a b) b' a' a b -> Proxy m v b' a' a b
join = bind id

mmap :: Functor m => (forall a. m a -> n a) -> Proxy m v b' a' a b -> Proxy n v b' a' a b
mmap f = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond b  r) = Respond b $ rec . r
  rec (M mr)         = M $ f $ rec <$> mr
  rec (Pure x)       = Pure x

mpure :: Monad m => m v -> Proxy m v b' a' a b
mpure ma = M $ Pure <$> ma

mbind :: (Monad m, Monad n) => (forall v. m v -> Proxy n v b' a' a b) -> Proxy m v b' a' a b -> Proxy n v b' a' a b
mbind amb = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond b  r) = Respond b $ rec . r
  rec (M mr)         = join $ amb $ rec <$> mr
  rec (Pure x)       = Pure x
