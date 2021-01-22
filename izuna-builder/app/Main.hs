module Main where

import           System.Environment

import           IzunaBuilder.Server (run)





main :: IO ()
main = do
  [port] <- getArgs
  run $ read @Int port
