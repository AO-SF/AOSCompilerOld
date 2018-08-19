module Compiler.Parser.Common where

import Control.Monad (void)
import Data.Char
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\r\t"

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

astSymbol :: String -> Parser String
astSymbol s = lexeme $ string s
