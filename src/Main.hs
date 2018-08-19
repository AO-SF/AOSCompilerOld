module Main where

import Compiler.Parser

import Text.Parsec.Char (anyChar)

main :: IO ()
main = print $ parseString "Hello, Haskell!"
