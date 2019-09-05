module Test where

import Data.Coerce

import Pipes
import Pipes.Core
import Data.Foldable
import Data.Functor.Identity
import Control.Monad
import Data.Profunctor
import Data.Tuple

import Optics
import Newtypes

flipped :: Iso (a, b) (c, d) (b, a) (d, c)
flipped = dimap swap swap

dumb :: Iso' (Int, String) Int
dumb = dimap (\(i, _) -> i) (\i -> (i, show i))

dumber :: Iso' (String, Int) Int
dumber = flipped . dumb

server :: Functor m => Server a Int m r
server = forever $ respond 42

client :: Client (String, Int) (String, Int) IO ()
client = forever $ do
  res <- request $ ("foo", 42)
  liftIO $ print res

main :: IO ()
main = runEffect $ (coerce $ dumber $ Downstream $ server) >>~ const client
