module Solutions.Y2015.Day03 (solution1, solution2) where

import Advent (Parser, Solution)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.HashSet qualified as Set
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char)

type Position = (Int, Int)

type SantaTracker = (Position, HashSet Position)

type Move = Position -> Position

parser :: Parser (NonEmpty Move)
parser = NE.some (north <|> south <|> east <|> west)
  where
    north = char '^' >> pure (\(x, y) -> (x, y + 1)) :: Parser Move
    south = char 'v' >> pure (\(x, y) -> (x, y - 1)) :: Parser Move
    east = char '>' >> pure (\(x, y) -> (x + 1, y)) :: Parser Move
    west = char '<' >> pure (\(x, y) -> (x - 1, y)) :: Parser Move

move :: [SantaTracker] -> Move -> [SantaTracker]
move [] _ = []
move ((p, ps) : otherSantas) m = otherSantas <> [liftA2 (,) id (`Set.insert` ps) $ m p]

houses :: Int -> NonEmpty Move -> HashSet Position
houses santas = sconcat . (:|) Set.empty . map snd . foldl' move (replicate santas santaTracker)
  where
    santaTracker = ((0, 0), one (0, 0))

solution1 :: Solution Int
solution1 input = Set.size . houses 1 <$> parse parser "" input

solution2 :: Solution Int
solution2 input = Set.size . houses 2 <$> parse parser "" input