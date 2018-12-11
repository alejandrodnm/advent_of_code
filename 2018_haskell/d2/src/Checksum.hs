module Checksum where

import Data.List
import Input

type Checksum = Integer

data ChecksumType = Two | Three | Both | None
  deriving Show

data ChecksumAgg = ChecksumAgg { two :: Integer
                               , three :: Integer }
                               deriving Show

checksumType :: BoxID -> ChecksumType
checksumType box =
  case intersect [2, 3] $ map length $ group $ sort box of
    [2] -> Two
    [3] -> Three
    [2, 3] -> Both
    _ -> None

aggregateChecksum :: ChecksumAgg -> BoxID -> ChecksumAgg
aggregateChecksum agg box =
  case checksumType box of
    Two ->  agg { two = two agg + 1 }
    Three -> agg { three = three agg + 1 }
    Both -> ChecksumAgg { two = two agg + 1, three = three agg + 1 }
    None -> agg

calculateChecksum :: ChecksumAgg -> Integer
calculateChecksum agg = two agg * three agg

getChecksum :: [BoxID] -> Checksum
getChecksum boxes =
  let initAgg = ChecksumAgg { two = 0, three = 0 }
      checksumAgg = foldl' aggregateChecksum initAgg boxes
  in calculateChecksum checksumAgg
