module Solutions.Y2019.Day01 (solution1, solution2) where

-- https://adventofcode.com/2019/day/1

import Advent (Parser, Solution, lexeme)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Text.Megaparsec (parse)
import Text.Megaparsec.Char.Lexer qualified as L

parser :: Parser (NonEmpty Int)
parser = NE.some $ lexeme L.decimal

singleFuel :: Int -> Int
singleFuel = subtract 2 . flip div 3

recursiveFuel :: Int -> Int
recursiveFuel mass
  | fuel <= 0 = 0
  | otherwise = fuel + recursiveFuel fuel
  where
    fuel = singleFuel mass

solution1 :: Solution
solution1 input = fromIntegral . sum . fmap singleFuel <$> parse parser "" input

solution2 :: Solution
solution2 input = fromIntegral . sum . fmap recursiveFuel <$> parse parser "" input
