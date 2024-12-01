module Solutions.Y2024.Day01 (solution1, solution2) where

-- https://adventofcode.com/2024/day/1

import Advent (Parser, Solution, lexeme)
import Data.HashMap.Strict (findWithDefault)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char.Lexer qualified as L

type Pair = (Int, Int)

parser :: Parser [Pair]
parser = many $ (,) <$> lexeme L.decimal <*> lexeme L.decimal

solution1 :: Solution Int
solution1 input = sum . uncurry (zipWith (\a b -> abs (a - b))) . bimap sort sort . unzip <$> parse parser "" input

solution2 :: Solution Int
solution2 input = sum . (\(as, bs) -> map (\a -> a * count a bs) as) . second counter . unzip <$> parse parser "" input
  where
    counter xs = fromList . map (\x -> (x, length . filter (== x) $ xs)) $ xs
    count = findWithDefault 0