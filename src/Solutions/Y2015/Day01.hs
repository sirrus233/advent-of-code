module Solutions.Y2015.Day01 (solution1, solution2) where

import Advent (Parser, Solution, length')
import Text.Megaparsec (eof, many, parse, (<|>))
import Text.Megaparsec.Char (char)

parser :: Parser [Integer]
parser = many (goingUp <|> goingDown) <* eof
  where
    goingUp = char '(' >> pure 1 :: Parser Integer
    goingDown = char ')' >> pure (-1) :: Parser Integer

solution1 :: Solution
solution1 input = sum <$> parse parser "" input

solution2 :: Solution
solution2 input = length' . takeWhile (>= 0) . scanl (+) 0 <$> parse parser "" input