module Solutions.Y2015.Day07 (solution1, solution2) where

-- https://adventofcode.com/2015/day/7

import Advent (Parser, Solution, lexeme, symbol)
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.HashMap.Strict qualified as Map
import Data.List.NonEmpty qualified as NE
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

evaluate :: Wire -> Circuit -> Word16
evaluate (Value a) _ = a
evaluate wire circuit = evalGate . fromMaybe (Buffer . Value $ 0) . Map.lookup wire $ circuit
  where
    evalGate :: LogicGate -> Word16
    evalGate (Not w) = complement $ evaluate w circuit
    evalGate (And w1 w2) = evaluate w1 circuit .&. evaluate w2 circuit
    evalGate (Or w1 w2) = evaluate w1 circuit .|. evaluate w2 circuit
    evalGate (LShift w n) = flip shiftL n $ evaluate w circuit
    evalGate (RShift w n) = flip shiftR n $ evaluate w circuit
    evalGate (Buffer w) = evaluate w circuit

solution1 :: Solution
solution1 input = fromIntegral . evaluate (Label "c") . fromList . toList <$> parse parser "" input

solution2 :: Solution
solution2 input = 0 <$ parse parser "" input
