module Solutions.Y2024.Day05 (solution1, solution2) where

-- https://adventofcode.com/2024/day/5

import Advent (Parser, Solution)
import Data.HashSet qualified as Set
import Text.Megaparsec (parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

type Page = Int

type Rule = (Page, Page)

type Pages = [Page]

type RuleBook = HashSet Rule

parser :: Parser (RuleBook, [Pages])
parser = curry (first fromList) <$> many rule <* char '\n' <*> many pages
  where
    rule = (,) <$> L.decimal <* char '|' <*> L.decimal <* char '\n' :: Parser Rule
    pages = L.decimal `sepBy` char ',' <* char '\n' :: Parser Pages

middlePage :: Pages -> Int
middlePage ps = head . fromList . drop (length ps `div` 2) $ ps

pageOrder :: RuleBook -> Page -> Page -> Ordering
pageOrder rb p1 p2
  | Set.member (p1, p2) rb = LT
  | Set.member (p2, p1) rb = GT
  | otherwise = EQ

isOrdered :: RuleBook -> Pages -> Bool
isOrdered rb ps = ps == sortBy (pageOrder rb) ps

solution1 :: Solution Int
solution1 input = sum . map middlePage . uncurry orderedPages <$> parse parser "" input
  where
    orderedPages rb = filter (isOrdered rb)

solution2 :: Solution Int
solution2 input = sum . map middlePage . uncurry sortedFromUnorderedPages <$> parse parser "" input
  where
    sortedFromUnorderedPages rb = map (sortBy (pageOrder rb)) . filter (not . isOrdered rb)
