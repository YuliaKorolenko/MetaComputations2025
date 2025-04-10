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
      let q = EConstant $
              ListC [ ListC [s "if", IntC 0, s "goto", IntC 3],
              ListC [s "right"],
              ListC [s "goto", IntC 0],
              ListC [s "write", IntC 1]]
      let right = EConstant $ lInt [1, 1, 0, 1, 0, 1]
      result <- eval turingInterpreter (M.fromList [("Q", q), ("Right", right)])
      case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right value -> value `shouldBe` EConstant (lInt [1, 1, 0, 1])
  describe "Turing maschine test 2" $ do
    it "fist 1 duplicates on left side" $ do
      let q = EConstant $
              ListC [
              ListC [s "if", IntC 0, s "goto", IntC 3],
              ListC [s "right"],
              ListC [s "goto", IntC 0],
              ListC [s "right"],
              ListC [s "if", IntC 0, s "goto", IntC 3],
              ListC [s "left"],
              ListC [s "write", IntC 1]]
      let right = lInt [0, 1, 0, 1, 1, 0, 0]
      result <- eval turingInterpreter (M.fromList [("Q", q), ("Right", EConstant right)])
      case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right value -> value `shouldBe` EConstant (lInt [1, 1, 0, 1, 1, 0, 0])