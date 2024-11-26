module Solutions.Y2015.Day05 (solution1, solution2) where

-- https://adventofcode.com/2015/day/5

import Advent (Parser, Solution, lexeme)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Control.Monad (foldM)
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (letterChar)

type ChPair = (Char, Char)

parser :: Parser (NonEmpty Text)
parser = NE.some (toText <$> lexeme (some letterChar))

meetsConditions :: [Text -> Bool] -> Text -> Bool
meetsConditions ps = and . sequenceA ps

isNice1 :: Text -> Bool
isNice1 = meetsConditions [threeVowels, doubleLetter, not . hasForbidden]
  where
    threeVowels :: Text -> Bool
    threeVowels = (>= 3) . T.length . T.filter (`T.elem` "aeiou")

    doubleLetter :: Text -> Bool
    doubleLetter = any (uncurry (==)) . liftA2 T.zip id T.tail

    hasForbidden :: Text -> Bool
    hasForbidden = any (`elem` forbidden) . liftA2 T.zip id T.tail
      where
        forbidden = [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')] :: [ChPair]

isNice2 :: Text -> Bool
isNice2 = meetsConditions [duplicatedPair, separatedDouble]
  where
    duplicatedPair :: Text -> Bool
    duplicatedPair = isLeft . foldM nextPair Map.empty . zip [0 ..] . liftA2 T.zip id T.tail
      where
        nextPair :: HashMap ChPair Int -> (Int, ChPair) -> Either () (HashMap ChPair Int)
        nextPair ps (idx, p) = case Map.lookup p ps of
          Nothing -> Right $ Map.insert p idx ps
          Just prevIdx
            | idx - prevIdx > 1 -> Left ()
            | otherwise -> Right ps

    separatedDouble :: Text -> Bool
    separatedDouble = any (uncurry (==)) . liftA2 T.zip id (T.drop 2)

solution1 :: Solution Int
solution1 input = length . NE.filter isNice1 <$> parse parser "" input

solution2 :: Solution Int
solution2 input = length . NE.filter isNice2 <$> parse parser "" input
