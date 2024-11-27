module Solutions.Y2015.Day09 (solution1, solution2) where

-- https://adventofcode.com/2015/day/9

import Advent (Parser, Solution, lexeme, symbol)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.Foldable (maximum, minimum)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Char.Lexer qualified as L

type City = Text

type Distance = Int

type Graph = HashMap City (HashMap City Distance)

parser :: Parser Graph
parser = buildGraph <$> NE.some edge
  where
    city = lexeme $ toText <$> some letterChar :: Parser City
    to = symbol "to" :: Parser Text
    equals = symbol "=" :: Parser Text
    dist = lexeme L.decimal :: Parser Distance
    edge = (,,) <$> city <* to <*> city <* equals <*> dist :: Parser (City, City, Distance)
    buildGraph = foldl' (\g (a, b, d) -> Map.insertWith (<>) b [(a, d)] . Map.insertWith (<>) a [(b, d)] $ g) []

distance :: Graph -> City -> City -> Distance
distance g from to = fromMaybe 0 $ Map.lookup from g >>= Map.lookup to

shortestRoute :: (forall t a. (Foldable t, Ord a) => (t a -> a)) -> Graph -> Int
shortestRoute optimizer g = optimizer . traverseAll (const 0) . fromList . Map.keys $ g
  where
    traverseAll :: (City -> Distance) -> HashSet City -> HashSet Int
    traverseAll d cities = Set.map (\nextCity -> go (d nextCity) (Set.delete nextCity cities) nextCity) cities

    go :: Distance -> HashSet City -> City -> Int
    go d unvisited current
      | null unvisited = d
      | otherwise = optimizer . traverseAll ((d +) . distance g current) $ unvisited

solution1 :: Solution Int
solution1 input = shortestRoute minimum <$> parse parser "" input

solution2 :: Solution Int
solution2 input = shortestRoute maximum <$> parse parser "" input
