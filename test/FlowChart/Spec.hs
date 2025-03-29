{-# LANGUAGE OverloadedStrings #-}

module FlowChart.Spec where

import Test.Hspec
import qualified Data.Map as M

import Interpret
import Dsl
import Ast
import TInterpreter (turingInterpreter)

testProgramVar :: Program
testProgramVar = program ["x", "y"]
                [bj $ returnCnst "x"]

testProgramXY = Program []
              [block ["x" #= (4 :: Int)],
               bja ["y" #= (6 :: Int)] $ returnCnst "x"]

maxProgram :: Program
maxProgram = program ["a", "b"]
 [
    bj (IF (BinOP PLUS (VAR (VarName "a")) (VAR (VarName "b"))) (Label "oneHundred") (Label "plus")),

    BasicBlock (Label "oneHundred")
      ["result" #= (100 :: Int)]
      (returnCnst "result"),

    BasicBlock (Label "plus")
      ["res" #= BinOP PLUS (VAR (VarName "a")) (VAR (VarName "b"))]
      (returnCnst "res")
  ]


spec :: Spec
spec = do
  describe "Test Program for Var" $ do
    it "correctly evaluates the value of 'x' in testProgramVar" $ do
      result <- eval testProgramVar (M.fromList [("x", IntConst 150), ("y", IntConst 22), ("c", IntConst 3)])
      case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right value -> value `shouldBe` IntConst 150
  describe "Test program return x" $ do
    it "correctly evaluates the return value of 'x' in testProgramX" $ do
      result <- eval testProgramXY M.empty
      case result of
          Left err -> putStrLn $ "Error: " ++ show err
          Right value -> value `shouldBe` IntConst 4
  describe "Test program for conditional evaluation of 'a' and 'b'" $ do
    it "correctly evaluates the return value of 'x' in testProgramX" $ do
      let a = 55
      let b = 45
      result <- eval maxProgram (M.fromList [("a", IntConst a), ("b", IntConst b)])
      case result of
          Left err -> putStrLn $ "Error: " ++ show err
          Right value -> do
            let expected = if a + b == 1 then 100 else a + b
            value `shouldBe` IntConst expected