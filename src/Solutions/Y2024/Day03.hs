module Solutions.Y2024.Day03 (solution1, solution2) where

-- https://adventofcode.com/2024/day/3

import Advent (Parser, Solution)
import Text.Megaparsec (anySingle, chunk, notFollowedBy, parse)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

parseMul :: Parser Int
parseMul = (*) <$ chunk "mul(" <*> L.decimal <* char ',' <*> L.decimal <* char ')'

parser1 :: Parser Int
parser1 = sum <$> many (garbage *> parseMul <* garbage)
  where
    garbage = void $ many (notFollowedBy parseMul >> anySingle) :: Parser ()

parser2 :: Parser Int
parser2 = sum <$> many (garbage *> (doMul <|> dontMul) <* garbage)
  where
    garbage = void $ many (notFollowedBy parseMul >> notFollowedBy pDo >> notFollowedBy pDont >> anySingle) :: Parser ()
    pDo = void $ chunk "do()" :: Parser ()
    pDont = void $ chunk "don't()" :: Parser ()
    doMul = sum <$ pDo <*> (notFollowedBy pDont >> many (garbage *> parseMul <* garbage)) :: Parser Int
    dontMul = const 0 <$ pDont <*> (notFollowedBy pDo >> many (garbage *> parseMul <* garbage)) :: Parser Int

solution1 :: Solution Int
solution1 = parse parser1 ""

solution2 :: Solution Int
solution2 = parse parser2 "" . ("do()" <>)
