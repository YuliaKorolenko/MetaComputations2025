module Main where

import qualified Data.Map.Strict as M

import Ast
import Interpret
import Dsl

testProgram :: Program
testProgram = program ["a", "b"]                               
  [ BasicBlock
      (Label "hi")                                    
      [constAssigment "x" 128]                                       
      (RETURN (VAR (VarName "x"))  
      )
  ]

maxProgram :: Program
maxProgram = program ["a", "b"]
 [
    blockLab "start" 
        [] 
        (IF (BinOP Plus (VAR (VarName "a")) (VAR (VarName "b"))) (Label "oneHundred") (Label "minus")),

    blockLab "oneHundred"
      [constAssigment "result" 100]
      (RETURN (VAR (VarName "result"))),

    blockLab "minus"
      [assigment "res" (BinOP Plus (VAR (VarName "a")) (VAR (VarName "b")))]
      (RETURN (VAR (VarName "res")))
  ]

testProgramVar :: Program
testProgramVar = Program [VarName "x", VarName "y"] []                              

main :: IO ()
main = do
    result <- eval maxProgram (M.fromList [("a", 100), ("b", 9)])
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right value -> putStrLn $ "Result: " ++ show value
    putStrLn "END"