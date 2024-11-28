module Solutions.Y2015.Day13 (solution1, solution2) where

-- https://adventofcode.com/2015/day/13

import Advent (Parser, Solution, lexeme)
import Data.Foldable (maximum)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Text.Megaparsec (chunk, parse)
import Text.Megaparsec.Char (char, letterChar)
import Text.Megaparsec.Char.Lexer qualified as L

type Name = Text

type Happiness = Int

type Score = ((Name, Name), Happiness)

type Scores = (HashMap (Name, Name) Happiness)

parser :: Parser Scores
parser = fromList <$> many placement
  where
    name = toText <$> lexeme (some letterChar) :: Parser Name
    firstName = name <* lexeme (chunk "would") :: Parser Name
    secondName = lexeme (chunk "happiness units by sitting next to") *> name :: Parser Name
    gain = lexeme (chunk "gain") *> lexeme L.decimal :: Parser Happiness
    lose = negate <$> (lexeme (chunk "lose") *> lexeme L.decimal) :: Parser Happiness
    end = lexeme (char '.')
    placement = (\n1 h n2 -> ((n1, n2), h)) <$> firstName <*> (gain <|> lose) <*> secondName <* end :: Parser Score

names :: Scores -> [Name]
names = hashNub . map fst . Map.keys

scorePlacements :: Scores -> Int
scorePlacements scores = maximum . NE.map score . NE.permutations . names $ scores
  where
    numPeople = length . names $ scores
    happiness (a, b, c) = fromJust $ liftA2 (+) (Map.lookup (b, a) scores) (Map.lookup (b, c) scores)
    score p = sum . map happiness . take numPeople . liftA3 zip3 id (drop 1) (drop 2) $ cycle p

addSelf :: Scores -> Scores
addSelf scores = fromList . concat $ [[mkScore n1 n2, mkScore n2 n1] | n1 <- newNames, n2 <- newNames, n1 /= n2]
  where
    newNames = (:) "Bradley" . names $ scores
    mkScore n1 n2 = case Map.lookup (n1, n2) scores of
      Nothing -> ((n1, n2), 0)
      Just h -> ((n1, n2), h)

solution1 :: Solution Int
solution1 input = scorePlacements <$> parse parser "" input

solution2 :: Solution Int
solution2 input = scorePlacements . addSelf <$> parse parser "" input
