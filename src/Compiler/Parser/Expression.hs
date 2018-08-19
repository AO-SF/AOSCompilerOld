module Compiler.Parser.Expression where

import Control.Monad (void)
import Data.Char
import Text.Parsec
import Text.Parsec.Combinator
import qualified Text.Parsec.Expr as E
import Text.Parsec.String (Parser)

import Compiler.Parser.Common

data Operator
  = OpMultiply
  | OpDivide
  | OpAdd
  | OpSubtract
  | OpLE
  | OpLT
  | OpGE
  | OpGT
  | OpEq
  | OpNEq
  deriving (Eq, Show)

data Expression
  = Number Integer
  | Symbol String
  | Add Expression
        Expression
  | Parens Expression
  | BinaryOp Expression
             Operator
             Expression
  deriving (Eq, Show)

expressionTable =
  [ [binary "*" OpMultiply E.AssocLeft]
  , [binary "/" OpDivide E.AssocLeft]
  , [binary "+" OpAdd E.AssocLeft]
  , [binary "-" OpSubtract E.AssocLeft]
  , [binary "<=" OpLE E.AssocLeft]
  , [binary "<" OpLT E.AssocLeft]
  , [binary ">=" OpGE E.AssocLeft]
  , [binary ">" OpGT E.AssocLeft]
  , [binary "==" OpEq E.AssocLeft]
  , [binary "!=" OpNEq E.AssocLeft]
  ]
  where
    binary name op = E.Infix (mkBinOp op <$ astSymbol name)
    mkBinOp op a = BinaryOp a op

expressionTerm :: Parser Expression
expressionTerm = number <|> symbol <|> parens

number :: Parser Expression
number = do
  n <- lexeme $ many1 digit
  return $ Number $ read n

symbol :: Parser Expression
symbol =
  lexeme $ do
    fc <- firstChar
    rest <- many nonFirstChar
    return $ Symbol (fc : rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

parens :: Parser Expression
parens = between (astSymbol "(") (astSymbol ")") expression

expression = E.buildExpressionParser expressionTable expressionTerm
