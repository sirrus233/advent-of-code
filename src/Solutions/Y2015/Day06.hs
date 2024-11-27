module Solutions.Y2015.Day06 (solution1, solution2) where

-- https://adventofcode.com/2015/day/6

import Advent (Parser, Solution, lexeme, symbol)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.HashMap.Strict qualified as Map
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as L

data Operation = On | Off | Toggle

type Light = (Int, Int)

type LightBox = (Light, Light)

initialGrid :: HashMap Int Int
initialGrid = Map.fromList $ map (,0) [0 .. 999999]

parser :: Parser (NonEmpty (Operation, LightBox))
parser = NE.some ((,) <$> op <*> lightBox)
  where
    turnOn = symbol "turn on" >> pure On :: Parser Operation
    turnOff = symbol "turn off" >> pure Off :: Parser Operation
    toggle = symbol "toggle" >> pure Toggle :: Parser Operation
    op = turnOn <|> turnOff <|> toggle :: Parser Operation
    light = (,) <$> lexeme L.decimal <* char ',' <*> lexeme L.decimal :: Parser Light
    lightBox = (,) <$> light <* symbol "through" <*> light :: Parser LightBox

unboxLights :: LightBox -> [Int]
unboxLights box = [x + 1000 * y | x <- xs, y <- ys]
  where
    xs = [(fst . fst) box .. (fst . snd) box]
    ys = [(snd . fst) box .. (snd . snd) box]

updateGrid :: (Operation -> (Int -> Int)) -> HashMap Int Int -> (Operation, LightBox) -> HashMap Int Int
updateGrid updater grid (op, box) = flipfoldl' (Map.adjust (updater op)) grid (unboxLights box)

getOp1 :: Operation -> (Int -> Int)
getOp1 = \case
  On -> const 1
  Off -> const 0
  Toggle -> \a -> if a == 0 then 1 else 0

getOp2 :: Operation -> (Int -> Int)
getOp2 = \case
  On -> (+) 1
  Off -> max 0 . subtract 1
  Toggle -> (+) 2

solution1 :: Solution Int
solution1 input = sum . Map.elems . foldl' (updateGrid getOp1) initialGrid <$> parse parser "" input

solution2 :: Solution Int
solution2 input = sum . Map.elems . foldl' (updateGrid getOp2) initialGrid <$> parse parser "" input
