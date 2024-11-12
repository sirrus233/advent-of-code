module Advent where

import Text.Megaparsec (ParseErrorBundle, ParsecT)

type Year = Int

type Day = Int

type Problem = Int

type SolutionId = (Year, Day, Problem)

type Parser a = ParsecT Void Text Identity a

type Solution = Text -> Either (ParseErrorBundle Text Void) Integer

dayString :: Day -> String
dayString d = if d < 10 then "0" <> show d else show d

getInputFilename :: SolutionId -> String
getInputFilename (y, d, _) = "data/" <> show y <> "/input" <> dayString d <> ".txt"

readInput :: SolutionId -> IO Text
readInput s = decodeUtf8 @Text <$> (readFileBS . getInputFilename $ s)

length' :: (Foldable t) => t a -> Integer
length' = fromIntegral . length
