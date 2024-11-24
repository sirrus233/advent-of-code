module Solutions.Y2015.Day07 (solution1, solution2) where

-- https://adventofcode.com/2015/day/7

import Advent (Parser, Solution, lexeme, symbol)
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.HashMap.Strict qualified as Map
import Data.Maybe (fromJust)
import Text.Megaparsec (parse, try)
import Text.Megaparsec.Char (lowerChar)
import Text.Megaparsec.Char.Lexer qualified as L

data Wire = Value Word16 | Label Text deriving (Eq)

data LogicGate = Not Wire | And Wire Wire | Or Wire Wire | LShift Wire Int | RShift Wire Int | Buffer Wire

type Circuit = HashMap Wire LogicGate

instance Hashable Wire where
  hashWithSalt :: Int -> Wire -> Int
  hashWithSalt s (Value a) = hashWithSalt s a
  hashWithSalt s (Label a) = hashWithSalt s a

parser :: Parser Circuit
parser = fromList <$> some (flip (,) <$> gate <* symbol "->" <*> wire)
  where
    wire = lexeme (value <|> label) :: Parser Wire
    gate = lexeme (notGate <|> andGate <|> orGate <|> lShiftGate <|> rShiftGate <|> buffer) :: Parser LogicGate

    value = Value <$> lexeme L.decimal :: Parser Wire
    label = Label <$> lexeme (toText <$> some lowerChar) :: Parser Wire
    notGate = Not <$> (symbol "NOT" *> wire) :: Parser LogicGate
    andGate = try $ And <$> wire <* symbol "AND" <*> wire :: Parser LogicGate
    orGate = try $ Or <$> wire <* symbol "OR" <*> wire :: Parser LogicGate
    lShiftGate = try $ LShift <$> wire <* symbol "LSHIFT" <*> lexeme L.decimal :: Parser LogicGate
    rShiftGate = try $ RShift <$> wire <* symbol "RSHIFT" <*> lexeme L.decimal :: Parser LogicGate
    buffer = Buffer <$> wire :: Parser LogicGate

probe :: Wire -> Circuit -> Maybe Word16
probe (Value a) _ = Just a
probe wire circuit = case Map.lookup wire circuit of
  Just (Buffer (Value a)) -> Just a
  _ -> Nothing

simulate :: Circuit -> Circuit
simulate circuit
  | all isSteady $ Map.elems circuit = circuit
  | otherwise = simulate $ evalGate <$> circuit
  where
    isSteady :: LogicGate -> Bool
    isSteady (Buffer (Value _)) = True
    isSteady _ = False

    evalGate :: LogicGate -> LogicGate
    evalGate gate = case gate of
      (Not w) -> eval $ complement <$> probe' w
      (And w1 w2) -> eval $ (.&.) <$> probe' w1 <*> probe' w2
      (Or w1 w2) -> eval $ (.|.) <$> probe' w1 <*> probe' w2
      (LShift w n) -> eval $ (`shiftL` n) <$> probe' w
      (RShift w n) -> eval $ (`shiftR` n) <$> probe' w
      (Buffer w) -> eval $ probe' w
      where
        probe' = flip probe circuit
        eval = maybe gate (Buffer . Value)

solveForLabel :: Text -> Circuit -> Word16
solveForLabel label = fromJust . probe (Label label) . simulate

updateFromSteadyLabel :: Text -> Text -> Circuit -> Circuit
updateFromSteadyLabel fromWire toWire circuit = Map.insert (Label toWire) (Buffer . Value $ updateSignal) circuit
  where
    updateSignal = solveForLabel fromWire circuit

solution1 :: Solution Int
solution1 input = fromIntegral . solveForLabel "a" <$> parse parser "" input

solution2 :: Solution Int
solution2 input = fromIntegral . solveForLabel "a" . updateFromSteadyLabel "a" "b" <$> parse parser "" input
