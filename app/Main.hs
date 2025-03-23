module Main where

import Control.Monad.State (StateT, put, MonadState(get), MonadTrans(lift), evalStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map.Strict as M

import Ast
import Interpret

testProgram :: Program
testProgram = Program []                               
  [ BasicBlock
      (Label "hi")                                    
      [Assigment (VarName "x") (CONST 128)]                                       
      ( RETURN (VAR (VarName "x"))  
      )
  ]

testProgramVar :: Program
testProgramVar = Program [VarName "x", VarName "y"] []                              

main :: IO ()
main = do
    result <- eval testProgram (M.fromList [("x", 1), ("y", 2), ("c", 3)])
    
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right value -> putStrLn $ "Result: " ++ show value
    putStrLn "END"