module Solutions.Y2024.Day02 (solution1, solution2) where

-- https://adventofcode.com/2024/day/2

import Advent (Parser, Solution)
import Text.Megaparsec (parse, sepEndBy1)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

data Direction = Decrease | Increase deriving (Eq)

data MetaData = MetaData {prevVal :: Maybe Int, prevDir :: Maybe Direction, mistakes :: Int}

parser :: Parser [[Int]]
parser = (L.decimal `sepEndBy1` char ' ') `sepEndBy1` char '\n'

direction :: Int -> Int -> Maybe Direction
direction a b
  | a < b = Just Increase
  | a > b = Just Decrease
  | otherwise = Nothing

isSafe :: Int -> [Int] -> Bool
isSafe limit = go (MetaData {prevVal = Nothing, prevDir = Nothing, mistakes = 0})
  where
    go :: MetaData -> [Int] -> Bool
    go (MetaData {mistakes}) [] = mistakes <= limit
    go (MetaData {mistakes}) [_] = mistakes <= limit
    go md@(MetaData {prevVal, prevDir, mistakes}) (a : b : ns)
      | mistakes > limit = False
      | isMistake = go dropThis (b : ns) || go dropNext (a : ns)
      | otherwise = go ok (b : ns)
      where
        dir = direction a b
        isMistake = abs (a - b) > 3 || isNothing dir || (isJust prevVal && isNothing prevDir) || (isJust prevDir && dir /= prevDir)
        ok = md {prevVal = Just a, prevDir = dir}
        dropThis = case prevVal of
          Nothing -> md {mistakes = mistakes + 1}
          Just p -> md {prevDir = direction p b, mistakes = mistakes + 1}
        dropNext = md {mistakes = mistakes + 1}

solution1 :: Solution Int
solution1 input = length . filter (isSafe 0) <$> parse parser "" input

solution2 :: Solution Int
solution2 input = length . filter (isSafe 1) <$> parse parser "" input
