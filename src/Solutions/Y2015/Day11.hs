module Solutions.Y2015.Day11 (solution1, solution2) where

-- https://adventofcode.com/2015/day/11

import Advent (Parser, Solution)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (letterChar)

parser :: Parser Text
parser = toText <$> some letterChar

solution1 :: Solution Int
solution1 input = 0 <$ parse parser "" input

solution2 :: Solution Int
solution2 input = 0 <$ parse parser "" input
