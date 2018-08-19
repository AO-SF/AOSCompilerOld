import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import System.IO
import Data.Either (isRight)

import Compiler.Parser

statementCount :: CodeModule -> Int
statementCount (CodeModule (StatementList stmts )) = length stmts

main :: IO ()
main = hspec $
  describe "Parser" $ do
    describe "Test scripts" $ do
      it "should parse arithmetic script without error" $ do
        handle <- openFile "./test/scripts/arithmetic" ReadMode
        contents <- hGetContents handle

        -- TODO: Don't repeat parseString contents
        (parseString contents) `shouldSatisfy` isRight
        let (Right code) = parseString contents

        (statementCount code) `shouldBe` (length $ lines contents)