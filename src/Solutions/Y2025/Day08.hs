module Solutions.Y2025.Day08 (solution1, solution2) where

-- https://adventofcode.com/2025/day/8

import Advent (Parser, Solution, lexeme)
import Data.HashSet qualified as Set
import Data.List (partition)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

type Point = (Integer, Integer, Integer)

type Network = [HashSet Point]

parser :: Parser [Point]
parser = some $ lexeme ((,,) <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal)

pairs :: [Point] -> [(Point, Point)]
pairs ps = ordNub [(min p1 p2, max p1 p2) | p1 <- ps, p2 <- ps, p1 /= p2]

distance :: Point -> Point -> Double
distance (a1, b1, c1) (a2, b2, c2) = sqrt $ d a1 a2 + d b1 b2 + d c1 c2
  where
    d a b = fromIntegral $ (a - b) ^ (2 :: Integer)

attach :: Network -> (Point, Point) -> Network
attach network (p1, p2) = uncurry go $ partition (liftA2 (||) (Set.member p1) (Set.member p2)) network
  where
    go [] circuit = fromList [p1, p2] : circuit
    go cs circuit = (Set.insert p2 . Set.insert p1 . Set.unions $ cs) : circuit

solution1 :: Solution Int
solution1 input =
  product
    . take 3
    . sortWith Down
    . fmap Set.size
    . foldl' attach []
    . take 1000
    . sortWith (uncurry distance)
    . pairs
    <$> parse parser "" input

solution2 :: Solution Integer
solution2 input = 0 <$ parse parser "" input
