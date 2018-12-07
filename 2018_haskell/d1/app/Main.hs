module Main where

import Data.String (lines)
import System.IO
import Paths_d1

getFreq :: [(Integer, String)] -> Integer
getFreq [] = 0
getFreq [(freq, "")] = freq
getFreq _ = 0

trimPlus :: String -> String
trimPlus ('+':xs) = xs
trimPlus a = a

aggregate :: Integer -> String -> Integer
aggregate acc readFreq =
  let trimmedF = trimPlus readFreq
      freq = getFreq (reads trimmedF :: [(Integer, String)])
   in acc + freq

main :: IO ()
main = do
  filePath <- getDataFileName "data/d1-input"
  handle <- openFile filePath ReadMode
  content <- hGetContents handle
  let frequencies = lines content
  print $ show $ foldl aggregate 0 frequencies
  hClose handle
  return ()
