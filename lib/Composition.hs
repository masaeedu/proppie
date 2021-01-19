{-# LANGUAGE TupleSections, PartialTypeSignatures, RecordWildCards #-}
module Composition where

import Prelude hiding (id, (.), zip)
import Control.Category ((<<<))
import Data.Bifunctor (Bifunctor(..))

data Nat = Z | S Nat

-- This is just the Proxy type from the library with an additional pair of
-- type parameters representing the number of requests and responses it
-- makes
data Proxy (i :: Nat) (o :: Nat) a' a b' b m r
  where
  Req ::  a' -> (a -> Proxy i o a' a b' b m r)  -> Proxy ('S i)     o  a' a b' b m r
  Res :: (b' -> (b,   Proxy i o a' a b' b m r)) -> Proxy     i  ('S o) a' a b' b m r
  Eff ::           m (Proxy i o a' a b' b m r)  -> Proxy     i      o  a' a b' b m r
  Fin :: r                                      -> Proxy    'Z     'Z  a' a b' b m r

deriving instance Functor m => Functor (Proxy i o a' a b' b m)

-- Polymorphize the input end of a pipe that never requests anything
ingnostic :: Functor m => Proxy 'Z o a' a b' b m r -> Proxy 'Z o x y b' b m r
ingnostic (Res f) = Res $ fmap ingnostic <<< f
ingnostic (Eff m  ) = Eff $ ingnostic <$> m
ingnostic (Fin r  ) = Fin r

-- Polymorphize the output end of a pipe that never returns a response
outgnostic :: Functor m => Proxy i 'Z a' a b' b m r -> Proxy i 'Z a' a x y m r
outgnostic (Req a f) = Req a $ outgnostic <<< f
outgnostic (Eff m  ) = Eff $ outgnostic <$> m
outgnostic (Fin r  ) = Fin r

husk :: Proxy 'Z 'Z a' a b' b m ()
husk = Fin ()

zip :: Functor m =>
  Proxy x o b' b c' c m r1 ->
  Proxy i x a' a b' b m r2 ->
  Proxy i o a' a c' c m (r1, r2)
Fin r   `zip` y       = outgnostic $ fmap (r, ) y
x       `zip` Fin r   = ingnostic  $ fmap (, r) x

Eff x   `zip` y       = Eff $ (`zip` y) <$> x
x       `zip` Eff y   = Eff $ (x `zip`) <$> y

Res f   `zip` y       = Res $ fmap (`zip` y) <<< f
x       `zip` Req y g = Req y $ (x `zip`) <<< g

Req x f `zip` Res g   = uncurry zip $ first f $ g x

id :: (Functor m, Monoid r) => Proxy 'Z 'Z a' a b' b m r
id = fmap (const mempty) husk

(.) :: (Functor m, Semigroup r) =>
  Proxy x o b' b c' c m r ->
  Proxy i x a' a b' b m r ->
  Proxy i o a' a c' c m r
pa . pb = fmap (uncurry (<>)) $ pa `zip` pb
