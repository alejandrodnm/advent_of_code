module D1 where

import Data.Maybe ( fromMaybe )

d1 :: IO ()
d1 = do
    dat <- getData
    print $ productOf2EntriesThatSum2020 dat
    print $ productOfEntriesThatSum2020 3 dat
    -- My first tought was doing list comprehension but realized that with a
    -- value of 1010 it might give a false positive since it will be read
    -- twice.
    -- Also I didn't wanted to create the subsequences.
    -- let nums = [1010]
    -- head [a*b | a <- nums, b <- nums, a + b == 2020] == 1020100

getData :: IO [Int]
getData = do
  content <- readFile "data/d1-input"
  let values = fmap read (lines content) :: [Int]
  pure values

productOf2EntriesThatSum2020 :: [Int] -> Int
productOf2EntriesThatSum2020 (x:xs) =
    case try2Values x xs of
      Just z -> z
      Nothing -> productOf2EntriesThatSum2020 xs
productOf2EntriesThatSum2020 [] = 0

try2Values :: Int -> [Int] -> Maybe Int
try2Values x (y:ys) = if x+y == 2020 then Just (x * y) else try2Values x ys
try2Values _ [] = Nothing

productOfEntriesThatSum2020 :: Int -> [Int] -> Int
productOfEntriesThatSum2020 n xs = fromMaybe 0 (f n [] xs)
    where
        f :: Int -> [Int] -> [Int] -> Maybe Int
        -- Stop at one instead of zero because we still have to compare
        -- against one value
        f 1 acc xs = tryValues acc xs
        f _ _ [] = Nothing
        f n acc (x:xs) =
            case f (n-1) (x:acc) xs of
              Nothing -> f n acc xs
              a -> a

tryValues :: [Int] -> [Int] -> Maybe Int
tryValues acc (x:xs) =
    let v = (x:acc) in
    if sum v == 2020 then Just (product v) else tryValues acc xs
tryValues _ [] = Nothing

--- Day 1: Report Repair ---

-- After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.

-- The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.

-- To save your vacation, you need to get all fifty stars by December 25th.

-- Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

-- Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.

-- Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

-- For example, suppose your expense report contained the following:

-- 1721
-- 979
-- 366
-- 299
-- 675
-- 1456

-- In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

-- Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?

-- Your puzzle answer was 989824.
-- --- Part Two ---

-- The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.

-- Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.

-- In your expense report, what is the product of the three entries that sum to 2020?

-- Your puzzle answer was 66432240.

-- Both parts of this puzzle are complete! They provide two gold stars: **

-- At this point, you should return to your Advent calendar and try another puzzle.

-- If you still want to see it, you can get your puzzle input.

-- You can also [Shareon Twitter Mastodon] this puzzle.
