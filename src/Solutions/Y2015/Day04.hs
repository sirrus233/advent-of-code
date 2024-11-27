module Solutions.Y2015.Day04 (solution1, solution2) where

-- https://adventofcode.com/2015/day/4

import Advent (Parser, Solution)
import Crypto.Hash.MD5 (hash)
import Data.ByteString qualified as BS
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (letterChar)

parser :: Parser ByteString
parser = encodeUtf8 . toText <$> some letterChar

encodeInteger :: Int -> ByteString
encodeInteger = encodeUtf8 @Text . show

findSpecialHash :: ([Word8] -> Bool) -> ByteString -> Int
findSpecialHash isSpecial key = (+) 1 . length . takeWhile (not . isSpecial) $ inputs
  where
    inputs = map (BS.unpack . hash . mappend key . encodeInteger) [1 ..]

solution1 :: Solution Int
solution1 input = findSpecialHash isSpecial <$> parse parser "" input
  where
    isSpecial = \case (0 : 0 : b3 : _) -> b3 < 16; _ -> False

solution2 :: Solution Int
solution2 input = findSpecialHash isSpecial <$> parse parser "" input
  where
    isSpecial = \case (0 : 0 : 0 : _) -> True; _ -> False
