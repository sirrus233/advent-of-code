module Solutions.Y2015.Day01 (solution1, solution2) where

import Advent (Parser, Solution, length')
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.List.NonEmpty qualified as NE
import Text.Megaparsec (eof, parse)
import Text.Megaparsec.Char (char)

parser :: Parser (NonEmpty Integer)
parser = NE.some (goingUp <|> goingDown) <* eof
  where
    goingUp = char '(' >> pure 1 :: Parser Integer
    goingDown = char ')' >> pure (-1) :: Parser Integer

solution1 :: Solution
solution1 input = sum <$> parse parser "" input

solution2 :: Solution
solution2 input = length' . NE.takeWhile (>= 0) . NE.scanl (+) 0 <$> parse parser "" input