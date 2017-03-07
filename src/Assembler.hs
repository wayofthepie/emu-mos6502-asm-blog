module Assembler where

import qualified Data.Text as T -- from the "text" package
import Text.Megaparsec  -- from the "megaparsec" package

type Parser = Parsec Dec T.Text

newtype Label = Label T.Text deriving Show

newtype IsImmediate = IsImmediate Bool deriving Show

data Operand = Operand IsImmediate T.Text deriving Show

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
operand = undefined

bytes :: Parser T.Text
bytes = undefined

labelAssign :: Parser Label
labelAssign = undefined

label :: Parser Label
label = undefined

mnemonic :: Parser Mnemonic
mnemonic = Mnemonic . T.pack <$> mnem
 where
  mnem = count 3 upperChar

byte :: Parser T.Text
byte = do
  high <- hexDigitChar
  low <- hexDigitChar
  pure $ T.pack [high,low]












