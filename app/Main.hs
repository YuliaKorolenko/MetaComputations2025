module Main where

import qualified Data.Map.Strict as M

import Ast
import Interpret
import Dsl
import TInterpreter(turingInterpreter)
import Control.Applicative (Alternative(empty))
                       

main :: IO ()
main = do
    -- let q = List [ List [StrConst "if", IntConst 0, StrConst "goto", IntConst 3],
                  --  List [StrConst "right"],
                  --  List [StrConst "goto", IntConst 0],
                  --  List [StrConst "write", IntConst 1]]
    let emptyq = lStr []
    let right = lInt [1, 1, 1, 1, 0, 1]
    result <- eval turingInterpreter (M.fromList [("Q", emptyq), ("Right", right)])
    case result of 
      Left err -> putStrLn $ "Error: " ++ show err
      Right value -> putStrLn $ "Value: " ++ show value