module Main where

import Input
import Checksum
import PrototypeFabric

main :: IO ()
main = do
  let boxes = getInput
      checksum = getChecksum boxes
      prototypeFabricBox = getPrototypeFabricBox boxes
  print checksum
  print prototypeFabricBox
