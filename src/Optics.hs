{-# LANGUAGE ConstraintKinds #-}

module Optics where

import Data.Profunctor
import Data.Profunctor.Strong
import Data.Profunctor.Choice

type Optic c s t a b = forall p. c p => p a b -> p s t

type Iso s t a b = Optic Profunctor s t a b
type Iso' s a = Iso s s a a

type Lens s t a b = Optic Strong s t a b
type Lens' s a = Lens s s a a

type Prism s t a b = Optic Choice s t a b
type Prism' s a = Prism s s a a

_1 :: Lens (a, x) (b, x) a b
_1 = first'

fromLeft :: Either a b -> Maybe a
fromLeft (Left a) = Just a
fromLeft _ = Nothing

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = dimap (maybe (Right ()) Left) fromLeft . left'
