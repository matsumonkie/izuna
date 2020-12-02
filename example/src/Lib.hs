module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

add :: Int -> Int -> Int
add x y =
  x + y

foo :: String
foo =
  let x = 42
  in show x
