module Solutions.Y2025.Day06 (solution1, solution2) where

-- https://adventofcode.com/2025/day/6

import Advent (Parser, Solution, lexeme, lexemeH)
import Data.List (zip4)
import Data.List.NonEmpty qualified as NE
import Relude.Unsafe (read)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char, eol, numberChar)
import Text.Megaparsec.Char.Lexer (decimal)

type Terms = NonEmpty Integer

type Operator = NonEmpty Integer -> Integer

type Worksheet = (NonEmpty Terms, NonEmpty Operator)

parser :: Parser Worksheet
parser = first NE.transpose <$> ((,) <$> NE.some1 pNumLine <*> pOpLine)
  where
    pOp = lexeme (sum <$ char '+' <|> product <$ char '*')
    pOpLine = NE.some1 pOp
    pNumLine = NE.some1 (lexemeH decimal) <* eol

parserCephalopod :: Parser (String, String, String, String, NonEmpty Operator)
parserCephalopod = (,,,,) <$> pNumLine <*> pNumLine <*> pNumLine <*> pNumLine <*> pOpLine
  where
    pOp = lexeme (sum <$ char '+' <|> product <$ char '*')
    pOpLine = NE.some1 pOp
    pNumLine = reverse <$> some (numberChar <|> char ' ') <* eol

cephalopod :: (String, String, String, String, NonEmpty Operator) -> Worksheet
cephalopod (l1, l2, l3, l4, ops) =
  (,ops) . fromList . fmap fromList . (\(w, n) -> n : w) . foldl' nextNum ([], []) $ zip4 l1 l2 l3 l4
  where
    nextNum :: ([[Integer]], [Integer]) -> (Char, Char, Char, Char) -> ([[Integer]], [Integer])
    nextNum (w, n) cs = case cs of
      (' ', ' ', ' ', ' ') -> (n : w, [])
      (c1, c2, c3, c4) -> (w, n' : n) where n' = read @Integer . filter (/= ' ') $ [c1, c2, c3, c4]

solve :: Worksheet -> NonEmpty Integer
solve (allTerms, ops) = (\(terms, op) -> op terms) <$> NE.zip allTerms ops

solution1 :: Solution Integer
solution1 input = sum . solve <$> parse parser "" input

solution2 :: Solution Integer
solution2 input = sum . solve . cephalopod <$> parse parserCephalopod "" input
