module Lib
    ( someFunc
    ) where

import Compiler.Parser

someFunc :: IO ()
someFunc = print $ parseString "3+3;"
