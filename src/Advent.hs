module Advent where

import Text.Megaparsec (ParseErrorBundle, ParsecT)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Year = Int

type Day = Int

type Problem = Int

type SolutionId = (Year, Day, Problem)

type Parser a = ParsecT Void Text Identity a

type Solution a = Text -> Either (ParseErrorBundle Text Void) a

data AnySolution = forall a. (Integral a) => AnySolution (Solution a)

dayString :: Day -> String
dayString d = if d < 10 then "0" <> show d else show d

getInputFilename :: SolutionId -> String
getInputFilename (y, d, _) = "data/" <> show y <> "/input" <> dayString d <> ".txt"

readInput :: SolutionId -> IO Text
readInput s = decodeUtf8 @Text <$> (readFileBS . getInputFilename $ s)

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer
