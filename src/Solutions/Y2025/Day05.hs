module Solutions.Y2025.Day05 (solution1, solution2) where

-- https://adventofcode.com/2025/day/5

import Advent (Parser, Solution, lexeme)
import Data.Ix (inRange)
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

type FoodId = Integer

type Range = (FoodId, FoodId)

parser :: Parser (NonEmpty Range, NonEmpty FoodId)
parser = (,) <$> NE.some1 pRange <* eol <*> NE.some1 (lexeme decimal)
  where
    pRange = (,) <$> decimal <* char '-' <*> decimal <* eol

countValidFoodIds :: NonEmpty Range -> NonEmpty FoodId -> Int
countValidFoodIds rs = sum . fmap (fromEnum . (\fid -> any (`inRange` fid) rs))

flattenRs :: NonEmpty Range -> NonEmpty Range
flattenRs (r :| []) = [r]
flattenRs (r1@(a1, b1) :| r2@(a2, b2) : rs)
  | b1 < a2 = r1 <| flattenRs (r2 :| rs)
  | a1 < a2 && b1 <= b2 = (a1, a2 - 1) <| flattenRs (r2 :| rs)
  | a1 <= a2 && b1 > b2 = flattenRs (r1 :| rs)
  | otherwise = flattenRs (r2 :| rs)

countRange :: Range -> Integer
countRange (a, b) = b - a + 1

solution1 :: Solution Int
solution1 input = uncurry countValidFoodIds <$> parse parser "" input

solution2 :: Solution Integer
solution2 input = sum . fmap countRange . flattenRs . NE.sort . fst <$> parse parser "" input
