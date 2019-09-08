{-# LANGUAGE ScopedTypeVariables #-}

module Pipes where

import Data.Void
import Data.Monoid (Ap(..))
import Control.Applicative (liftA2)
import qualified Data.Bifunctor as B

data Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r)
    | Respond (b' -> (b, Proxy a' a b' b m r))
    | M (m (Proxy a' a b' b m r))
    | Pure r

-- Instances

deriving via (Ap (Proxy a' a b' b m) r)
  instance (Functor m, Semigroup r) => Semigroup (Proxy a' a b' b m r)

deriving via (Ap (Proxy a' a b' b m) r)
  instance (Functor m, Monoid r) => Monoid (Proxy a' a b' b m r)

fmap :: Functor m => (x -> y) -> Proxy b' a' a b m x -> Proxy b' a' a b m y
fmap f = rec
  where
  rec (Request a' x) = Request a' $ rec . x
  rec (Respond    r) = Respond $ (rec <$>) . r
  rec (M mr)         = M (rec <$> mr)
  rec (Pure x)       = Pure $ f x

pure :: x -> Proxy b' a' a b m x
pure = Pure

liftA2 :: Functor m => (x -> y -> z) -> Proxy b' a' a b m x -> Proxy b' a' a b m y -> Proxy b' a' a b m z
liftA2 f = rec
  where
  rec (Request a' r) y = Request a' $ flip rec y . r
  rec (Respond    r) y = Respond $ (flip rec y <$>) . r
  rec (M mr)         y = M $ flip rec y <$> mr
  rec (Pure x)       y = Pipes.fmap (f x) y

bind :: Functor m => (x -> Proxy b' a' a b m y) -> Proxy b' a' a b m x -> Proxy b' a' a b m y
bind amb = rec
  where
  rec (Request a' r) = Request a' $ rec . r
  rec (Respond r   ) = Respond $ (rec <$>) . r
  rec (M mr)         = M $ rec <$> mr
  rec (Pure x)       = amb x

instance Functor m => Functor (Proxy a' a b' b m)
  where
  fmap = Pipes.fmap

instance Functor m => Applicative (Proxy a' a b' b m)
  where
  pure = Pipes.pure
  liftA2 = Pipes.liftA2

instance Functor m => Monad (Proxy a' a b' b m)
  where
  (>>=) = flip bind

-- Aliases

type Server i o = Proxy Void () i o
type Server' r = Server r r

type Client i o = Proxy o i () Void
type Client' r = Client r r

type Effect = Proxy Void () () Void

-- Combinators

request :: a' -> Proxy a' a b' b m a
request a' = Request a' Pure

respond :: (b' -> b) -> Proxy a' a b' b m b'
respond f = Respond $ \b' -> (f b', Pure b')

connect :: Functor m => Proxy b' b c' c m r -> Proxy a' a b' b m r -> Proxy a' a c' c m r
-- pair a request in the client with the next response in the server
connect    (Request c cb) (Respond s)    = uncurry connect $ B.first cb (s c)
connect r1@(Request _ _)  (Request s cb) = Request s $ connect r1 . cb
connect r1@(Request _ _)  (M mr)         = M $ connect r1 <$> mr
connect _  (Pure v)                      = Pure v
-- skip everything in the client
connect    (Respond r) s                 = Respond $ (flip connect s <$>) . r
connect    (M mr) s                      = M $ flip connect s <$> mr
connect    (Pure v) _                    = Pure v

(<<<) :: Functor m => Proxy b' b c' c m r -> Proxy a' a b' b m r -> Proxy a' a c' c m r
(<<<) = connect

(>>>) :: Functor m => Proxy a' a b' b m r -> Proxy b' b c' c m r -> Proxy a' a c' c m r
(>>>) = flip (<<<)

runEffect :: Monad m => Effect m r -> m r
runEffect (Request r _) = absurd r
runEffect (Respond cb ) = absurd . fst . cb $ ()
runEffect (M mr       ) = mr >>= runEffect
runEffect (Pure v     ) = Prelude.pure v
