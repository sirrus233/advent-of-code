module Solutions.Y2024.Day05 (solution1, solution2) where

-- https://adventofcode.com/2024/day/5

import Advent (Parser, Solution)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Text.Megaparsec (parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

type Rule = (Int, Int)

type Pages = [Int]

type RuleBook = HashMap Int (HashSet Int)

parser :: Parser ([Rule], [Pages])
parser = (,) <$> many rule <* char '\n' <*> many pages
  where
    rule = (,) <$> L.decimal <* char '|' <*> L.decimal <* char '\n' :: Parser Rule
    pages = L.decimal `sepBy` char ',' <* char '\n' :: Parser Pages

mkRuleBook :: [Rule] -> RuleBook
mkRuleBook = foldl' (\rb (a, b) -> Map.insertWith Set.union a (one b) rb) Map.empty

middlePage :: Pages -> Int
middlePage ps = head . fromList . drop (length ps `div` 2) $ ps

isOrdered :: RuleBook -> Pages -> Bool
isOrdered rb = go Set.empty
  where
    go :: HashSet Int -> Pages -> Bool
    go _ [] = True
    go seen (p : ps) = case forbiddenPages of
      Nothing -> go seen' ps
      Just forbidden -> null (Set.intersection seen forbidden) && go seen' ps
      where
        forbiddenPages = Map.lookup p rb
        seen' = Set.insert p seen

solution1 :: Solution Int
solution1 input = sum . (\(rs, ps) -> map middlePage . filter (isOrdered rs) $ ps) . first mkRuleBook <$> parse parser "" input

solution2 :: Solution Int
solution2 input = 0 <$ parse parser "" input
