{-# LANGUAGE GADTs #-}

-- http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf

module FunList where

data FunList a b t
  where
  Done :: t -> FunList a b t
  More :: a -> FunList a b (b -> t) -> FunList a b t

instance Functor (FunList a b)
  where
  fmap f (Done v)   = Done $ f v
  fmap f (More a l) = More a $ (f .) <$> l

instance Applicative (FunList a b)
  where
  pure = Done
  (Done f)    <*> fv = f <$> fv
  (More a ff) <*> fv = More a $ (flip <$> ff) <*> fv

single :: a -> FunList a b b
single x = More x $ Done id

fuse :: FunList b b t -> t
fuse (Done t) = t
fuse (More x l) = fuse l x

type FunListTraversal s t a b = s -> FunList a b t

fromTraversable :: Traversable t => t a -> FunList a b (t b)
fromTraversable = traverse single
