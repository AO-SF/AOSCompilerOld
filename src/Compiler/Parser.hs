module Compiler.Parser where

import Control.Monad (void)
import Data.Char
import Text.Parsec
import Text.Parsec.Combinator
import qualified Text.Parsec.Expr as E
import Text.Parsec.String (Parser)

import Compiler.Parser.Common
import Compiler.Parser.Expression

newtype StatementList =
  StatementList [Expression]
  deriving (Eq, Show)

newtype CodeModule =
  CodeModule StatementList
  deriving (Eq, Show)

endOfStatement :: Parser Char
endOfStatement = char ';'

statementList :: Parser StatementList
statementList = do
  statements <- many (expression <* endOfStatement)
  return $ StatementList statements

codeModule :: Parser CodeModule
codeModule = CodeModule <$> statementList

parseString :: String -> Either ParseError CodeModule
parseString = parse codeModule ""
