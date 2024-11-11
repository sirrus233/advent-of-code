module Solutions.Y2015.Day03 (solution1, solution2) where

import Advent (Parser, Solution, length')
import Data.Containers.ListUtils (nubOrd)
import Text.Megaparsec (eof, many, parse, (<|>))
import Text.Megaparsec.Char (char)

data Position = Position {x :: Integer, y :: Integer} deriving (Eq, Ord)

type Move = Position -> Position

parser :: Parser [Move]
parser = many (north <|> south <|> east <|> west) <* eof
  where
    north = char '^' >> pure (\(Position {x, y}) -> Position {x, y = y + 1}) :: Parser Move
    south = char 'v' >> pure (\(Position {x, y}) -> Position {x, y = y - 1}) :: Parser Move
    east = char '>' >> pure (\(Position {x, y}) -> Position {x = x + 1, y}) :: Parser Move
    west = char '<' >> pure (\(Position {x, y}) -> Position {x = x - 1, y}) :: Parser Move

move :: [Position] -> Move -> [Position]
move [] _ = []
move (p : ps) m = ps <> [m p]

houses :: Int -> [Move] -> [Position]
houses santas = concat . scanl move initialPositions
  where
    initialPosition = Position {x = 0, y = 0}
    initialPositions = initialPosition : replicate (santas - 1) initialPosition

solution1 :: Solution
solution1 input = length' . nubOrd . houses 1 <$> parse parser "" input

solution2 :: Solution
solution2 input = length' . nubOrd . houses 2 <$> parse parser "" input