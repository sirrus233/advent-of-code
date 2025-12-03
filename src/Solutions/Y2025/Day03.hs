module Solutions.Y2025.Day03 (solution1, solution2) where

-- https://adventofcode.com/2025/day/3

import Advent (Parser, Solution, lexeme)
import Data.Char (digitToInt)
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (digitChar)

type Battery = Integer

type Joltage = Integer

type Bank = NonEmpty Battery

parser :: Parser (NonEmpty Bank)
parser = NE.some1 . lexeme . NE.some1 $ fromIntegral . digitToInt <$> digitChar

maxJoltage :: Int -> Bank -> Joltage
maxJoltage winSize bs = toJoltage . foldl' updateFromWindow initialJoltage $ windows
  where
    initialJoltage = replicate winSize 0
    windows = getZipList . traverse ZipList . take winSize . tails . toList $ bs
    updateFromWindow [] [] = []
    updateFromWindow (j : js) (w : ws)
      | w > j = w : ws
      | otherwise = j : updateFromWindow js ws
    updateFromWindow _ _ = error "Size mismatch between result array and sliding window."
    toJoltage as = sum [10 ^ i * a | (i, a) <- zip [0 :: Integer ..] (reverse as)]

solution1 :: Solution Integer
solution1 input = sum . fmap (maxJoltage 2) <$> parse parser "" input

solution2 :: Solution Integer
solution2 input = sum . fmap (maxJoltage 12) <$> parse parser "" input
