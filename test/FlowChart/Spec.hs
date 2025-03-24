{-# LANGUAGE OverloadedStrings #-}

module FlowChart.Spec where

import Test.Hspec
import qualified Data.Map as M

import Interpret
import Ast
import Dsl
import Ast 

testProgramVar :: Program
testProgramVar = Program [VarName "x", VarName "y"] []

spec :: Spec
spec = do
  describe "Test Program" $ do
    it "evaluates testProgram correctly" $ do
      result <- eval testProgramVar (M.fromList [("x", 1), ("y", 2), ("c", 3)])
      case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right value -> value `shouldBe` 128
