{-# LANGUAGE ConstraintKinds, ScopedTypeVariables #-}

module Optics where

import Data.Coerce

import Data.Profunctor
import Data.Profunctor.Strong
import Data.Profunctor.Choice
import Data.Tuple

type Optic p s t a b = p a b -> p s t

type Iso s t a b = forall p. Profunctor p => Optic p s t a b
type Iso' s a = Iso s s a a

type Lens s t a b = forall p. Strong p => Optic p s t a b
type Lens' s a = Lens s s a a

type Colens s t a b = forall p. Costrong p => Optic p s t a b
type Colens' s a = Colens s s a a

type Prism s t a b = forall p. Choice p => Optic p s t a b
type Prism' s a = Prism s s a a

_1 :: Lens (a, x) (b, x) a b
_1 = first'

fromLeft :: Either a b -> Maybe a
fromLeft (Left a) = Just a
fromLeft _ = Nothing

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = dimap (maybe (Right ()) Left) fromLeft . left'

newtype Re p s t a b = Re (p b a -> p t s)

instance Profunctor p => Profunctor (Re p s t)
  where
  dimap f g (Re r) = Re $ r . dimap g f

instance Strong p => Costrong (Re p s t)
  where
  unfirst (Re r) = Re $ r . first'

instance Costrong p => Strong (Re p s t)
  where
  first' (Re r) = Re $ r . unfirst

re' :: Optic (Re p a b) s t a b -> Optic p b a t s
re' l = coerce $ l $ Re id

re :: Lens s t a b -> Colens b a t s
re l = re' l

re_1 :: Colens' a (a, b)
re_1 = re _1

flipped :: Iso (a, b) (c, d) (b, a) (d, c)
flipped = dimap swap swap
