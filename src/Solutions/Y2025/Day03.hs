module Solutions.Y2025.Day03 (solution1, solution2) where

-- https://adventofcode.com/2025/day/3

import Advent (Parser, Solution, lexeme)
import Data.Char (digitToInt)
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (digitChar)

type Battery = Int

type Joltage = Int

type Bank = NonEmpty Battery

parser :: Parser (NonEmpty Bank)
parser = NE.some1 . lexeme . NE.some1 $ digitToInt <$> digitChar

maxJoltage :: Bank -> Joltage
maxJoltage bs = toJoltage . foldl' updateBest (0, 0) $ zip (toList bs) (tail bs)
  where
    updateBest (a, b) (i, j)
      | i > a = (i, j)
      | j > b = (a, j)
      | otherwise = (a, b)
    toJoltage (a, b) = 10 * a + b

solution1 :: Solution Int
solution1 input = sum . fmap maxJoltage <$> parse parser "" input

solution2 :: Solution Int
solution2 input = 0 <$ parse parser "" input
