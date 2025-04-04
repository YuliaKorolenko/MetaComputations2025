{-# LANGUAGE OverloadedStrings #-}

module TuringMachine.Spec where

import Test.Hspec
import qualified Data.Map as M

import Interpret
import Dsl
import Ast
import TInterpreter (turingInterpreter)


specTM :: Spec
specTM = do
  describe "Turing maschine test" $ do
    it "change first 1 to zero" $ do
      let q = List [ List [StrConst "if", IntConst 0, StrConst "goto", IntConst 3],
              List [StrConst "right"],
              List [StrConst "goto", IntConst 0],
              List [StrConst "write", IntConst 1]]
      let right = lInt [1, 1, 0, 1, 0, 1]
      result <- eval turingInterpreter (M.fromList [("Q", q), ("Right", right)])
      case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right value -> value `shouldBe` lInt [1, 1, 0, 1]
  describe "Turing maschine test 2" $ do
    it "fist 1 duplicates on left side" $ do
      let q = List [ 
              List [StrConst "if", IntConst 0, StrConst "goto", IntConst 3],
              List [StrConst "right"],
              List [StrConst "goto", IntConst 0],
              List [StrConst "right"],
              List [StrConst "if", IntConst 0, StrConst "goto", IntConst 3],
              List [StrConst "left"],
              List [StrConst "write", IntConst 1]]
      let right = lInt [0, 1, 0, 1, 1, 0, 0] 
      result <- eval turingInterpreter (M.fromList [("Q", q), ("Right", right)])
      case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right value -> value `shouldBe` lInt [1, 1, 0, 1, 1, 0, 0]