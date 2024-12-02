module Solutions.Y2024.Day02 (solution1, solution2) where

-- https://adventofcode.com/2024/day/2

import Advent (Parser, Solution)
import Text.Megaparsec (parse, sepEndBy1)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

data Direction = Decrease | Increase deriving (Eq)

data MetaData = MetaData {prevVal :: Maybe Int, mistakes :: Int}

parser :: Parser [[Int]]
parser = (L.decimal `sepEndBy1` char ' ') `sepEndBy1` char '\n'

direction :: Int -> Int -> Maybe Direction
direction a b
  | a < b = Just Increase
  | a > b = Just Decrease
  | otherwise = Nothing

isSafe :: Int -> [Int] -> Bool
isSafe limit = go (MetaData {prevVal = Nothing, mistakes = 0})
  where
    go :: MetaData -> [Int] -> Bool
    go (MetaData {mistakes}) [] = mistakes <= limit
    go (MetaData {mistakes}) [_] = mistakes <= limit
    go (MetaData {mistakes}) [a, b] = mistakes <= limit && a /= b && abs (a - b) <= 3
    go md@(MetaData {mistakes}) (a : b : c : ns)
      | mistakes > limit = False
      | isValid = go md (b : c : ns)
      | otherwise = go invalidMd (b : c : ns) || go invalidMd (a : c : ns) || go invalidMd (a : b : ns)
      where
        isValid = a /= b && abs (a - b) <= 3 && direction a b == direction b c
        invalidMd = md {mistakes = mistakes + 1}

solution1 :: Solution Int
solution1 input = length . filter (isSafe 0) <$> parse parser "" input

solution2 :: Solution Int
solution2 input = length . filter (isSafe 1) <$> parse parser "" input
