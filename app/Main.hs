module Main where

import qualified Data.Map.Strict as M

import Ast
import Interpret
import Dsl
import TInterpreter(turingInterpreter)
import Control.Applicative (Alternative(empty))
import Division
import System.IO (print)
import Dsl (program, block)
import Division (generateStaticVars)
import Ast 

testProgramXY = program ["k"]
              [block ["x" #= (4 :: Int)],
               block ["f" #= "k"],
               bja ["y" #= (6 :: Int)] $ returnCnst "x"]


main :: IO ()
main = do
    let res = generateStaticVars turingInterpreter [VarName "Right"]
    print res
