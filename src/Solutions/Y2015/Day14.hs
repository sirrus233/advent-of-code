module Solutions.Y2015.Day14 (solution1, solution2) where

-- https://adventofcode.com/2015/day/14

import Advent (Parser, Solution, lexeme)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.Char (isDigit)
import Data.Foldable1 (maximum)
import Text.Megaparsec (parse, takeWhileP)
import Text.Megaparsec.Char.Lexer qualified as L

data ReindeerState = Flying | Resting

type Position = Int

type Score = Int

type Speed = Int

type Time = Int

data Reindeer = Reindeer
  { reinState :: ReindeerState,
    tRemain :: Time,
    pos :: Position,
    score :: Score,
    fSpeed :: Speed,
    fTime :: Time,
    rTime :: Time
  }

mkReindeer :: Int -> Int -> Int -> Reindeer
mkReindeer fSpeed fTime rTime = Reindeer {reinState = Flying, tRemain = fTime, pos = 0, score = 0, fSpeed, fTime, rTime}

parser :: Parser (NonEmpty Reindeer)
parser = NE.some (mkReindeer <$> nextInt <*> nextInt <*> nextInt)
  where
    text = lexeme $ takeWhileP Nothing (not . isDigit) :: Parser Text
    nextInt = text *> lexeme L.decimal <* text :: Parser Int

updateReindeer :: Reindeer -> Reindeer
updateReindeer r@(Reindeer {reinState, tRemain, pos, score = _, fSpeed, fTime, rTime}) = case reinState of
  Resting | tRemain > 1 -> r {tRemain = tRemain - 1}
  Resting -> r {reinState = Flying, tRemain = fTime}
  Flying | tRemain > 1 -> r {tRemain = tRemain - 1, pos = pos + fSpeed}
  Flying -> r {reinState = Resting, tRemain = rTime, pos = pos + fSpeed}

evalReindeer :: Time -> NonEmpty Reindeer -> NonEmpty Reindeer
evalReindeer t rs
  | t == 0 = rs
  | otherwise = evalReindeer (t - 1) scoredReindeer
  where
    updatedReindeer = fmap updateReindeer rs
    bestPos = maximum . fmap pos $ updatedReindeer
    scoredReindeer = fmap (\r -> if pos r == bestPos then r {score = score r + 1} else r) updatedReindeer

solution1 :: Solution Int
solution1 input = maximum . fmap pos . evalReindeer 2503 <$> parse parser "" input

solution2 :: Solution Int
solution2 input = maximum . fmap score . evalReindeer 2503 <$> parse parser "" input
