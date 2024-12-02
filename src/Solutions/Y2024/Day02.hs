module Solutions.Y2024.Day02 (solution1, solution2) where

-- https://adventofcode.com/2024/day/2

import Advent (Parser, Solution, lexeme)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (manyTill, parse, sepEndBy, sepEndBy1)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

data Status = Safe | Dampened | Unsafe

data Polarity = Negative | Positive

parser :: Parser [[Int]]
parser = (L.decimal `sepEndBy1` char ' ') `sepEndBy1` char '\n'

isSafe :: [Int] -> Bool
isSafe [] = True
isSafe [_] = True
isSafe (x0 : x1 : xs) = all (\a -> abs a <= 3) z && (all (> 0) z || all (< 0) z)
  where
    z = zipWith (-) (x0 : x1 : xs) (x1 : xs)

solution1 :: Solution Int
solution1 input = length . filter isSafe <$> parse parser "" input

solution2 :: Solution Int
solution2 input = 0 <$ parse parser "" input
