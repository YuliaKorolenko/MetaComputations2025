module Main where

import qualified Data.Map.Strict as M

import Ast
import Interpret
import Dsl


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

main :: IO ()
main = do
    result <- eval maxProgram (M.fromList [("a", 100), ("b", 9)])
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right value -> putStrLn $ "Result: " ++ show value
    putStrLn "END"