module Solutions.Y2015.Day10 (solution1, solution2) where

-- https://adventofcode.com/2015/day/10

import Advent (Parser, Solution, applyN)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.Char (digitToInt)
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (digitChar)

parser :: Parser (NonEmpty Int)
parser = fmap digitToInt <$> NE.some digitChar

lookAndSay :: NonEmpty Int -> NonEmpty Int
lookAndSay = sconcat . fmap (\ds -> length ds :| [head ds]) . NE.group1

solution1 :: Solution Int
solution1 input = length . applyN 40 lookAndSay <$> parse parser "" input

solution2 :: Solution Int
solution2 input = length . applyN 50 lookAndSay <$> parse parser "" input
