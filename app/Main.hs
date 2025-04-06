module Main where

import qualified Data.Map.Strict as M

import Ast
import Interpret
import InterpretOp
import Dsl
import TInterpreter
import Division
import System.IO
import Prelude
import Interpret (reduceExpr, reduceOp)

testProgramXY = program ["k"]
              [block ["x" #= (4 :: Int)],
               bl "h" [],
               bja ["y" #= (6 :: Int)] $ returnCnst "x"]


maxProgram :: Program
maxProgram = program ["a", "b"]
 [
    bj (if' (pl (v "a") (v "b")) "oneHundred" "plus"),

    BasicBlock (Label "oneHundred")
      ["result" #= (100 :: Int)]
      (returnCnst "result"),

    BasicBlock (Label "plus")
      ["res" #= pl (v "a") (v "b")]
      (returnCnst "res")
  ]


-- reduceOp (EConstant (ExprC expr)) (EConstant (ListC constants)) 

main :: IO ()
main = do
    let expr = EBinOP Plus (v "a") (v "b")
    let varnames = EConstant (ListC [ListC [ExprC (EVar (VarName "a")), IntC 1]])
    let reducedExpr = reduceOp expr varnames
    let expectedExpr = IntC 1 
    print reducedExpr
