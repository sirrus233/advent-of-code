module Solutions.Y2015.Day04 (solution1, solution2) where

-- https://adventofcode.com/2015/day/4

import Advent (Parser, Solution, length')
import Crypto.Hash.MD5 (hash)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word (Word8)
import Text.Megaparsec (anySingle, eof, manyTill, parse, try)
import Text.Megaparsec.Char (eol)

parser :: Parser ByteString
parser = T.encodeUtf8 . T.pack <$> manyTill anySingle (try eol) <* eof

encodeInteger :: Integer -> ByteString
encodeInteger = T.encodeUtf8 . T.show

findSpecialHash :: ([Word8] -> Bool) -> ByteString -> Integer
findSpecialHash isSpecial key = (+) 1 . length' . takeWhile (not . isSpecial) $ inputs
  where
    inputs = map (BS.unpack . hash . mappend key . encodeInteger) [1 ..]

solution1 :: Solution
solution1 input = findSpecialHash isSpecial <$> parse parser "" input
  where
    isSpecial (0 : 0 : b3 : _) = b3 < 16
    isSpecial _ = False

solution2 :: Solution
solution2 input = findSpecialHash isSpecial <$> parse parser "" input
  where
    isSpecial (0 : 0 : 0 : _) = True
    isSpecial _ = False
