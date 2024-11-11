module Advent where

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.IO.Utf8 qualified as TIO
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, ParsecT)
import Text.Printf (printf)

type Year = Int

type Day = Int

type Problem = Int

type SolutionId = (Year, Day, Problem)

type Parser a = ParsecT Void Text Identity a

type Solution = Text -> Either (ParseErrorBundle Text Void) Integer

dayString :: Day -> String
dayString d = if d < 10 then "0" <> show d else show d

getInputFilename :: SolutionId -> String
getInputFilename (y, d, _) = printf "data/%d/input%s.txt" y (dayString d)

readInput :: SolutionId -> IO Text
readInput = TIO.readFile . getInputFilename

length' :: forall (t :: Type -> Type) a. (Foldable t) => t a -> Integer
length' = fromIntegral . length
