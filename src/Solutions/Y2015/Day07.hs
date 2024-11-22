module Solutions.Y2015.Day07 (solution1, solution2) where

-- https://adventofcode.com/2015/day/7

import Advent (Parser, Solution, lexeme, symbol)
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Text.Megaparsec (parse, try)
import Text.Megaparsec.Char (lowerChar)
import Text.Megaparsec.Char.Lexer qualified as L

data Wire = Value Word16 | Label String deriving (Eq)

data LogicGate = Not Wire | And Wire Wire | Or Wire Wire | LShift Wire Int | RShift Wire Int | Buffer Wire

type Connection = (Wire, LogicGate)

type Circuit = HashMap Wire LogicGate

instance Hashable Wire where
  hashWithSalt :: Int -> Wire -> Int
  hashWithSalt s (Value a) = hashWithSalt s a
  hashWithSalt s (Label a) = hashWithSalt s a

parser :: Parser (NonEmpty Connection)
parser = NE.some1 (flip (,) <$> gate <* symbol "->" <*> wire)
  where
    wire = lexeme (value <|> label) :: Parser Wire
    gate = lexeme (notGate <|> andGate <|> orGate <|> lShiftGate <|> rShiftGate <|> buffer) :: Parser LogicGate

    value = Value <$> lexeme L.decimal :: Parser Wire
    label = Label <$> lexeme (some lowerChar) :: Parser Wire
    notGate = Not <$> (symbol "NOT" *> wire) :: Parser LogicGate
    andGate = try $ And <$> wire <* symbol "AND" <*> wire :: Parser LogicGate
    orGate = try $ Or <$> wire <* symbol "OR" <*> wire :: Parser LogicGate
    lShiftGate = try $ LShift <$> wire <* symbol "LSHIFT" <*> lexeme L.decimal :: Parser LogicGate
    rShiftGate = try $ RShift <$> wire <* symbol "RSHIFT" <*> lexeme L.decimal :: Parser LogicGate
    buffer = Buffer <$> wire :: Parser LogicGate

propagate :: Circuit -> Circuit
propagate circuit = Map.map evalGate circuit
  where
    probe :: Wire -> Maybe Word16
    probe (Value a) = Just a
    probe wire = case Map.lookup wire circuit of
      Just (Buffer (Value a)) -> Just a
      _ -> Nothing

    evalGate :: LogicGate -> LogicGate
    evalGate gate = case gate of
      (Not w) -> eval $ complement <$> probe w
      (And w1 w2) -> eval $ (.&.) <$> probe w1 <*> probe w2
      (Or w1 w2) -> eval $ (.|.) <$> probe w1 <*> probe w2
      (LShift w n) -> eval $ (`shiftL` n) <$> probe w
      (RShift w n) -> eval $ (`shiftR` n) <$> probe w
      (Buffer w) -> eval $ probe w
      where
        eval = maybe gate (Buffer . Value)

readSteadyState :: String -> [Circuit] -> Word16
readSteadyState label cs = fromJust . getSteady . Map.lookup (Label label) $ steadyState
  where
    isSteady (Just (Buffer (Value _))) = True
    isSteady _ = False

    steadyState = head . fromList . dropWhile (not . isSteady . Map.lookup (Label label)) $ cs

    getSteady (Just (Buffer (Value a))) = Just a
    getSteady _ = Nothing

solution1 :: Solution
solution1 input = fromIntegral . readSteadyState "a" . iterate propagate . fromList . toList <$> parse parser "" input

solution2 :: Solution
solution2 input = fromIntegral . readSteadyState "a" . iterate propagate . Map.adjust (const $ Buffer . Value $ 3176) (Label "b") . fromList . toList <$> parse parser "" input
