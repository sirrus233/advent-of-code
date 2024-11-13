module Solutions.Y2015.Day06 (solution1, solution2) where

-- https://adventofcode.com/2015/day/6

import Advent (Parser, Solution)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.IntMap.Strict qualified as Map
import Text.Megaparsec (chunk, eof, parse)
import Text.Megaparsec.Char (char, eol, spaceChar)
import Text.Megaparsec.Char.Lexer qualified as L

data Operation = On | Off | Toggle

type Light = (Int, Int)

type LightBox = (Light, Light)

initialGrid :: IntMap Int
initialGrid = Map.fromList $ map (,0) [0 .. 999999]

parser :: Parser (NonEmpty (Operation, LightBox))
parser = ((,) <$> parseOp <* spaceChar <*> parseLightBox) `NE.sepEndBy1` eol <* eof
  where
    parseOn = chunk "turn on" >> pure On :: Parser Operation
    parseOff = chunk "turn off" >> pure Off :: Parser Operation
    parseToggle = chunk "toggle" >> pure Toggle :: Parser Operation
    parseOp = parseOn <|> parseOff <|> parseToggle :: Parser Operation
    parseLight = (,) <$> L.decimal <* char ',' <*> L.decimal :: Parser Light
    parseLightBox = (,) <$> parseLight <* chunk " through " <*> parseLight :: Parser LightBox

unboxLights :: LightBox -> [Int]
unboxLights box = [x + 1000 * y | x <- xs, y <- ys]
  where
    xs = [(fst . fst) box .. (fst . snd) box]
    ys = [(snd . fst) box .. (snd . snd) box]

getOp1 :: Operation -> (Int -> Int)
getOp1 op = case op of
  On -> const 1
  Off -> const 0
  Toggle -> \a -> if a == 0 then 1 else 0

getOp2 :: Operation -> (Int -> Int)
getOp2 op = case op of
  On -> (+) 1
  Off -> max 0 . subtract 1
  Toggle -> (+) 2

updateGrid :: (Operation -> (Int -> Int)) -> IntMap Int -> (Operation, LightBox) -> IntMap Int
updateGrid updater grid (op, box) = flipfoldl' (Map.adjust (updater op)) grid (unboxLights box)

solution1 :: Solution
solution1 input = fromIntegral . sum . Map.elems . foldl' (updateGrid getOp1) initialGrid <$> parse parser "" input

solution2 :: Solution
solution2 input = fromIntegral . sum . Map.elems . foldl' (updateGrid getOp2) initialGrid <$> parse parser "" input
