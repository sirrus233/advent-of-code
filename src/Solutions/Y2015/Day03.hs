module Solutions.Y2015.Day03 (solution1, solution2) where

import Advent (Parser, Solution, length')
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (eof, parse)
import Text.Megaparsec.Char (char)

type Position = (Integer, Integer)

type Move = Position -> Position

parser :: Parser (NonEmpty Move)
parser = NE.some (north <|> south <|> east <|> west) <* eof
  where
    north = char '^' >> pure (\(x, y) -> (x, y + 1)) :: Parser Move
    south = char 'v' >> pure (\(x, y) -> (x, y - 1)) :: Parser Move
    east = char '>' >> pure (\(x, y) -> (x + 1, y)) :: Parser Move
    west = char '<' >> pure (\(x, y) -> (x - 1, y)) :: Parser Move

move :: NonEmpty Position -> Move -> NonEmpty Position
move (p :| ps) m = NE.prependList ps [m p]

houses :: Int -> NonEmpty Move -> NonEmpty Position
houses santas = sconcat . NE.scanl move initialPositions
  where
    initialPosition = (0, 0)
    initialPositions = initialPosition :| replicate (santas - 1) initialPosition

solution1 :: Solution
solution1 input = length' . hashNub . toList . houses 1 <$> parse parser "" input

solution2 :: Solution
solution2 input = length' . hashNub . toList . houses 2 <$> parse parser "" input