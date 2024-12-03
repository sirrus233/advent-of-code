module Solutions.Y2024.Day01 (solution1, solution2) where

-- https://adventofcode.com/2024/day/1

import Advent (Parser, Solution, lexeme)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char.Lexer qualified as L

type Pair = (Int, Int)

parser :: Parser [Pair]
parser = many $ (,) <$> lexeme L.decimal <*> lexeme L.decimal

solution1 :: Solution Int
solution1 input = sum . uncurry (zipWith (\a b -> abs (a - b))) . bimap sort sort . unzip <$> parse parser "" input

solution2 :: Solution Int
solution2 input = uncurry go . bimap sort sort . unzip <$> parse parser "" input
  where
    go :: [Int] -> [Int] -> Int
    go [] _ = 0
    go left@(x : _) right = (x * length ns * length count) + go as bs
      where
        (ns, as) = span (== x) left
        (count, bs) = span (== x) . dropWhile (< x) $ right
