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

server :: Monad m => Server a (Int, Int) m r
server = forever $ respond (42, 42)

server' :: Monad m => Server a Int m r
server' = coerce $ reLens _1 $ Downstream $ server

client :: Client Int Int IO ()
client = forever $ do
  res <- request $ 42
  liftIO $ print res

main :: IO ()
main = runEffect $ server' >>~ const client
