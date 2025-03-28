module Main where

import qualified Data.Map.Strict as M

import Ast
import Interpret
import Dsl
import TInterpreter
                       

main :: IO ()
main = do
    let q = lStr ["if 0 goto 3", "right", "goto 0", "write 1"]
    -- let emptyq = lStr []
    let right = lInt [1, 1, 1, 1, 0, 1]
    result <- eval turingInterpreter (M.fromList [("Q", q), ("Right", right)])
    case result of 
      Left err -> putStrLn $ "Error: " ++ show err
      Right value -> putStrLn $ "Value: " ++ show value