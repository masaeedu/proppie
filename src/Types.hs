module Types where

data Proxy m v b' a' a b
  = Request a' (a  -> Proxy m v b' a' a b)
  | Respond b  (b' -> Proxy m v b' a' a b)
  | M (m (Proxy m v b' a' a b))
  | Pure v
