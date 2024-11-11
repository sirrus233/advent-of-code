module Solutions.Y2015.Day02 (solution1, solution2) where

import Advent (Parser, Solution)
import Text.Megaparsec (eof, parse, sepEndBy)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer qualified as L

type Gift = (Integer, Integer, Integer)

parser :: Parser [Gift]
parser = gift `sepEndBy` eol <* eof
  where
    gift = (,,) <$> L.decimal <* char 'x' <*> L.decimal <* char 'x' <*> L.decimal

measureGift :: Gift -> Integer
measureGift (l, w, h) = surfaceArea + smallestArea
  where
    area1 = l * w
    area2 = l * h
    area3 = h * w
    surfaceArea = 2 * (area1 + area2 + area3)
    smallestArea = minimum ([area1, area2, area3] :: [Integer])

measureRibbon :: Gift -> Integer
measureRibbon (l, w, h) = smallestPerimeter + volume
  where
    perimeter1 = 2 * (l + w)
    perimeter2 = 2 * (l + h)
    perimeter3 = 2 * (h + w)
    smallestPerimeter = minimum ([perimeter1, perimeter2, perimeter3] :: [Integer])
    volume = l * w * h

solution1 :: Solution
solution1 input = sum . map measureGift <$> parse parser "" input

solution2 :: Solution
solution2 input = sum . map measureRibbon <$> parse parser "" input