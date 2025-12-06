module Solutions.Y2025.Day06 (solution1, solution2) where

-- https://adventofcode.com/2025/day/6

import Advent (Parser, Solution, lexeme, lexemeH)
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char, eol)
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

solve :: Worksheet -> NonEmpty Integer
solve (allTerms, ops) = (\(terms, op) -> op terms) <$> NE.zip allTerms ops

cephalopod :: Terms -> Terms
cephalopod = fromList . fmap (fromMaybe 0 . readMaybe) . transpose . fmap (reverse . show) . toList

solution1 :: Solution Integer
solution1 input = sum . solve <$> parse parser "" input

solution2 :: Solution Integer
solution2 input = sum . solve . traceShowWith fst . first (fmap cephalopod) <$> parse parser "" input
