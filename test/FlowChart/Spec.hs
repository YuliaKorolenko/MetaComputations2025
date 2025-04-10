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
    bj (if' (pl (v "a") (v "b")) "oneHundred" "plus"),

    BasicBlock (Label "oneHundred")
      ["result" #= (100 :: Int)]
      (returnCnst "result"),

    BasicBlock (Label "plus")
      ["res" #= pl (v "a") (v "b")]
      (returnCnst "res")
  ]


spec :: Spec
spec = do
  describe "Test Program for Var" $ do
    it "correctly evaluates the value of 'x' in testProgramVar" $ do
      result <- eval testProgramVar (M.fromList [("x", EConstant (IntC 150)), ("y", EConstant (IntC 22)), ("c", EConstant (IntC 3))])
      case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right value -> value `shouldBe` EConstant (IntC 150)
  describe "Test program return x" $ do
    it "correctly evaluates the return value of 'x' in testProgramX" $ do
      result <- eval testProgramXY M.empty
      case result of
          Left err -> putStrLn $ "Error: " ++ show err
          Right value -> value `shouldBe` EConstant (IntC 4)
  describe "Test program for conditional evaluation of 'a' and 'b'" $ do
    it "correctly evaluates the return value of 'x' in testProgramX" $ do
      let a = 55
      let b = 45
      result <- eval maxProgram (M.fromList [("a", EConstant $ IntC a), ("b", EConstant $ IntC b)])
      case result of
          Left err -> putStrLn $ "Error: " ++ show err
          Right value -> do
            let expected = if a + b == 1 then 100 else a + b
            value `shouldBe` EConstant (IntC expected)