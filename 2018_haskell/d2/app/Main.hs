module Main where

import Input
import Checksum

main :: IO ()
main = do
  let boxes = getInput
      checksum = getChecksum boxes
      prototypeFabricBox = getPrototypeFabricBox boxes
  print checksum
  print prototypeFabricBox
