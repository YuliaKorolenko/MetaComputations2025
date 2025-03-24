{-# LANGUAGE OverloadedStrings #-}

module FlowChart.Spec where

import Test.Hspec
import qualified Data.Map as M

import Interpret
import Dsl
import Ast 

testProgramVar :: Program
testProgramVar = program ["x", "y"] 
                [blockJump [] $ returnCnst "x"]

testProgramXY = Program [] 
              [block [constAssigment "x" 4],
              blockJump [constAssigment "y" 6] $ returnCnst "x"]

maxProgram :: Program
maxProgram = program ["a", "b"]
 [
    blockJump [] 
      (IF (BinOP Plus (VAR (VarName "a")) (VAR (VarName "b"))) (Label "oneHundred") (Label "plus")),

    blockLabJump "oneHundred"
      [constAssigment "result" 100]
      (returnCnst "result"),

    blockLabJump "plus"
      [assigment "res" (BinOP Plus (VAR (VarName "a")) (VAR (VarName "b")))]
      (returnCnst "res")
  ]
              

spec :: Spec
spec = do
  describe "Test Program for Var" $ do
    it "correctly evaluates the value of 'x' in testProgramVar" $ do
      result <- eval testProgramVar (M.fromList [("x", 150), ("y", 22), ("c", 3)])
      case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right value -> value `shouldBe` 150
  describe "Test program return x" $ do
    it "correctly evaluates the return value of 'x' in testProgramX" $ do
      result <- eval testProgramXY M.empty
      case result of
          Left err -> putStrLn $ "Error: " ++ show err
          Right value -> value `shouldBe` 4
  describe "Test program for conditional evaluation of 'a' and 'b'" $ do
    it "correctly evaluates the return value of 'x' in testProgramX" $ do
      let a = 55
      let b = 45 
      result <- eval maxProgram (M.fromList [("a", a), ("b", b)])
      case result of
          Left err -> putStrLn $ "Error: " ++ show err
          Right value -> do 
            let expected = if a + b == 1 then 100 else a + b
            value `shouldBe` expected
            


