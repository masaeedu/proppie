module Test where

import Data.Coerce

import Pipes (Proxy(..), Server', Client', Effect, request, respond, connect, runEffect)
import Data.Foldable
import Data.Functor.Identity
import Control.Monad
import Control.Applicative
import Data.Profunctor
import Data.Tuple

import Optics
import Newtypes
import ExtraClasses

server :: Functor m => Server' Int m r
server = forever $ respond (* 2)

client :: Int -> Client' Int IO ()
client i = replicateM_ 10 $ do
  res <- request $ i
  mpure $ print res

effect :: Effect IO ()
effect = connect (client 1) server

main :: IO ()
main = runEffect $ connect (client 1) server
