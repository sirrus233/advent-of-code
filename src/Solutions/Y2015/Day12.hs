module Solutions.Y2015.Day12 (solution1, solution2) where

-- https://adventofcode.com/2015/day/12

import Advent (Solution)
import Data.Aeson (Value (..), decode)
import Data.Aeson.KeyMap (KeyMap, elems)
import Data.Maybe (fromJust)
import Data.Scientific (floatingOrInteger)
import Data.Vector qualified as V
import Text.Megaparsec (parse, takeRest)

type ObjectReducer = KeyMap Value -> Int

redOk :: ObjectReducer
redOk = sum . map (sumJson redOk) . elems

redNotOk :: ObjectReducer
redNotOk obj
  | String "red" `elem` obj = 0
  | otherwise = sum . map (sumJson redNotOk) . elems $ obj

sumJson :: ObjectReducer -> Value -> Int
sumJson _ (Number n) = either (const 0) fromIntegral (floatingOrInteger n :: Either Float Integer)
sumJson objReducer (Array arr) = sum . V.map (sumJson objReducer) $ arr
sumJson objReducer (Object obj) = objReducer obj
sumJson _ _ = 0

solution1 :: Solution Int
solution1 input = fromJust . (pure . sumJson redOk <=< decode) . encodeUtf8 <$> parse takeRest "" input

solution2 :: Solution Int
solution2 input = fromJust . (pure . sumJson redNotOk <=< decode) . encodeUtf8 <$> parse takeRest "" input
