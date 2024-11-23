module Solutions.Y2015.Day08 (solution1, solution2) where

-- https://adventofcode.com/2015/day/8

import Advent (Parser, Solution)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Text.Megaparsec (anySingleBut, chunk, eof, parse, try)
import Text.Megaparsec.Char (char, eol, hexDigitChar)

parser1 :: Parser Int
parser1 = sum <$> NE.some1 line <* eof
  where
    quoteLit = 0 <$ char '\"' :: Parser Int
    backslash = 1 <$ chunk "\\\\" :: Parser Int
    quote = 1 <$ chunk "\\\"" :: Parser Int
    hex = 1 <$ chunk "\\x" <* hexDigitChar <* hexDigitChar :: Parser Int
    other = 1 <$ anySingleBut '\"' :: Parser Int
    stringChar = try backslash <|> try quote <|> try hex <|> other :: Parser Int
    line = sum <$> (quoteLit *> many stringChar <* quoteLit <* eol) :: Parser Int

parser2 :: Parser Int
parser2 = sum <$> NE.some1 line <* eof
  where
    quoteLit = 3 <$ char '\"' :: Parser Int
    backslash = 4 <$ chunk "\\\\" :: Parser Int
    quote = 4 <$ chunk "\\\"" :: Parser Int
    hex = 5 <$ chunk "\\x" <* hexDigitChar <* hexDigitChar :: Parser Int
    other = 1 <$ anySingleBut '\"' :: Parser Int
    stringChar = try backslash <|> try quote <|> try hex <|> other :: Parser Int
    line = (\q1 cs q2 -> q1 + sum cs + q2) <$> quoteLit <*> many stringChar <*> quoteLit <* eol :: Parser Int

originalCodeLength :: Text -> Int
originalCodeLength = sum . map T.length . lines

solution1 :: Solution
solution1 input = fromIntegral . (-) (originalCodeLength input) <$> parse parser1 "" input

solution2 :: Solution
solution2 input = fromIntegral . subtract (originalCodeLength input) <$> parse parser2 "" input
