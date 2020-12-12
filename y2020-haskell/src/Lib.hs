module Lib
    ( getData
    ) where

getData :: String -> IO [String]
getData f = do
  content <- readFile ("data/" ++ f)
  pure $ lines content
