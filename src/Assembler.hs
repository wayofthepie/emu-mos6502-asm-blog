module Assembler where

import Control.Monad (void)
import qualified Data.Text as T       -- from the "text" package
import Text.Megaparsec hiding (Label, label) -- from the "megaparsec" package
import qualified Text.Megaparsec.Lexer as L -- from the "megaparsec" package

type Parser = Parsec Dec T.Text

newtype Label = Label T.Text deriving (Eq, Show)

newtype IsImmediate = IsImmediate Bool deriving (Eq, Show)

data Operand = Operand IsImmediate T.Text deriving (Eq, Show)

newtype Mnemonic = Mnemonic T.Text deriving (Eq, Show)

newtype Var = Var Label  deriving Show

newtype Val = Val Operand deriving Show

data LabelOrOperand = Lbl Label | Op Operand deriving Show

data Expr
  = Instruction (Maybe Label) Mnemonic (Maybe LabelOrOperand)
  | Assignment Var Val
  deriving Show

expression :: Parser Expr
expression = undefined

assignment :: Parser Expr
assignment = undefined

instruction :: Parser Expr
instruction = undefined

operand :: Parser Operand
operand = lexeme $ Operand
  <$> (option (IsImmediate False) (char '#' >> (pure $ IsImmediate True)))
  <*> bytes

bytes :: Parser T.Text
bytes = do
  char '$'
  firstByte <- byte
  anotherByte <- option T.empty byte
  pure $ T.append firstByte anotherByte

labelAssign :: Parser Label
labelAssign = lexeme $ label <* char ':'

label :: Parser Label
label = lexeme $ Label . T.pack <$> ((:) <$> letterChar <*>  many alphaNumChar)

mnemonic :: Parser Mnemonic
mnemonic = lexeme $ Mnemonic . T.pack <$> mnem
 where
  mnem = count 3 upperChar

byte :: Parser T.Text
byte = do
  high <- hexDigitChar
  low <- hexDigitChar
  pure $ T.pack [high,low]

-- | Eats space and comments! Yum!
spaceEater :: Parser ()
spaceEater = L.space
  (void spaceChar)
  (L.skipLineComment ";")
  (L.skipBlockComment "/*" "*/")

-- | A single unit, removes trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceEater

