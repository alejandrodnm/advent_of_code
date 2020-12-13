module Main where

import D1 ( d1 )
import D2 ( d2 )
import D3 ( d3 )
import D4 ( d4 )

main :: IO ()
main = do
    putStrLn "day 1"
    d1
    putStrLn "day 2"
    d2
    putStrLn "day 3"
    d3
    putStrLn "day 4"
    d4
