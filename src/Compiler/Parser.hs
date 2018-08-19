module Compiler.Parser where

import Data.Char
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import qualified Text.Parsec.Expr as E

data Operator = OpMultiply
              | OpDivide
              | OpAdd
              | OpSubtract
              | OpLT
              | OpGT
              deriving (Eq, Show)

data Expression = Number Integer
                | Add Expression Expression
                | Parens Expression
                | BinaryOp Expression Operator Expression
                deriving (Eq, Show)

expressionTable = [ [binary "*" OpMultiply E.AssocLeft]
                  , [binary "/" OpDivide   E.AssocLeft]
                  , [binary "+" OpAdd      E.AssocLeft]
                  , [binary "-" OpSubtract E.AssocLeft]
                  , [binary "<" OpLT       E.AssocLeft]
                  , [binary ">" OpGT       E.AssocLeft]
                  ]
                  where
                    binary name op = E.Infix (mkBinOp op <$ symbol name)
                    mkBinOp op a = BinaryOp a op

expressionTerm :: Parser Expression
expressionTerm = number <|> parens

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\r\t"

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

number :: Parser Expression
number = do
  n <- lexeme $ many1 digit
  return $ Number $ read n

parens :: Parser Expression
parens = between (symbol "(") (symbol ")") expression

symbol :: String -> Parser String
symbol s = lexeme $ string s

expression = E.buildExpressionParser expressionTable expressionTerm

parseString :: String -> Either ParseError Expression
parseString = parse expression ""
