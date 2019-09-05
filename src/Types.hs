module Types where

data Proxy b' a' a b m v
  = Request a' (a  -> Proxy b' a' a b m v)
  | Respond b  (b' -> Proxy b' a' a b m v)
  | M (m (Proxy b' a' a b m v))
  | Pure v
