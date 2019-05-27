module Lib
    ( someFunc, doubleSmallNumber
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100
                         then x
                         else x * 2
