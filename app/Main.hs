module Main where

import qualified Data.Map.Strict as M

import Ast
import Interpret
import Dsl
import TInterpreter
import Division
import System.IO 
import Prelude

testProgramXY = program ["k"]
              [block ["x" #= (4 :: Int)],
               bl "h" [],
               bja ["y" #= (6 :: Int)] $ returnCnst "x"]


main :: IO ()
main = do
    let result = lookupOp (ProgramC testProgramXY) (s "h")
    print result
