module Solutions.Y2015.Day14 (solution1, solution2) where

-- https://adventofcode.com/2015/day/14

import Advent (Parser, Solution, lexeme)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.Char (isDigit)
import Data.Foldable (maximum)
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (parse, takeWhileP)
import Text.Megaparsec.Char.Lexer qualified as L

data ReindeerState = Flying | Resting

type Position = Int

data ReindeerStats = ReindeerStats Speed Time Time

type Speed = Int

type Time = Int

data Reindeer = Reindeer ReindeerState Position ReindeerStats

parser :: Parser (NonEmpty Reindeer)
parser = fmap (Reindeer Flying 0) <$> NE.some (ReindeerStats <$> nextInt <*> nextInt <*> nextInt)
  where
    text = lexeme $ takeWhileP Nothing (not . isDigit) :: Parser Text
    nextInt = text *> lexeme L.decimal <* text :: Parser Int

evalReindeer :: Time -> Reindeer -> Int
evalReindeer t (Reindeer Resting pos stats@(ReindeerStats _ _ restTime))
  | t <= restTime = pos
  | otherwise = evalReindeer (t - restTime) (Reindeer Flying pos stats)
evalReindeer t (Reindeer Flying pos stats@(ReindeerStats speed flyTime _))
  | t <= flyTime = pos + (speed * t)
  | otherwise = evalReindeer (t - flyTime) (Reindeer Resting (pos + (speed * flyTime)) stats)

solution1 :: Solution Int
solution1 input = maximum . NE.map (evalReindeer 2503) <$> parse parser "" input

solution2 :: Solution Int
solution2 input = 0 <$ parse parser "" input
