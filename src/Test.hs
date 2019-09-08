module Test where

import Pipes (Server', Client', request, respond, (>>>), runEffect)
import Data.Foldable
import Control.Monad
import Optics()
import Newtypes()
import ExtraClasses

server :: Functor m => Server' Int m r
server = forever $ respond (* 2)

client :: Int -> Client' Int IO ()
client l = traverse_ go [0..l]
  where
  go i = do
    res <- request i
    mpure $ print res

main :: IO ()
main = runEffect $ server >>> client 10
{-
0
2
4
6
8
10
12
14
16
18
20
-}
