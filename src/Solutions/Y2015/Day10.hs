module Solutions.Y2015.Day10 (solution1, solution2) where

-- https://adventofcode.com/2015/day/10

import Advent (Parser, Solution)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (parse)

parser :: Parser Int
parser = pure 0

solution1 :: Solution Int
solution1 input = 0 <$ parse parser "" input

solution2 :: Solution Int
solution2 input = 0 <$ parse parser "" input
