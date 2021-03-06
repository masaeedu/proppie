module Main where

import Data.Coerce

import Pipes (Server', Client, request, respond, (>>>), runEffect)
import Data.Foldable
import Control.Monad
import Optics
import Newtypes
import ExtraClasses

server :: Functor m => Server' Int m r
server = forever $ respond (* 2)

client :: Show s => Int -> Client s (Int, Int) IO ()
client l = traverse_ go [0..l]
  where
  go i = do
    res <- request (i, i)
    mpure $ print res

main :: IO ()
main = runEffect $ (coerce $ swapped . _1 $ Downstream $ server) >>> client 10
{-
*Test> main
(0,0)
(1,2)
(2,4)
(3,6)
(4,8)
(5,10)
(6,12)
(7,14)
(8,16)
(9,18)
(10,20)
-}
