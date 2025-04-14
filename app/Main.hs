module Main where

import qualified Data.Map.Strict as M

import Ast
import Interpret
import InterpretOp
import Dsl
import TInterpreter
import Division
import System.IO
import Prelude
import Interpret (reduceExpr, reduceOp)
import Mix
import Ast
import Prelude (print)


maxSmall :: Program
maxSmall = program ["a", "b", "c"]
    [bl "initial" [ "tail_a" #= tl (v "b"), "head_a" #= hd (v "a")]]

-- maxProgramWithConditions :: Program
-- maxProgramWithConditions = program ["a", "b", "c"]
--     [bl "initial" ["a" #= v "b"],
--     bl "tail_and_head_a" [ "tail_a" #= tl (v "a"), "head_a" #= hd (v "a")],
--     bl "tail_and_head_b" ["tail_b" #= tl (v "b"), "head_b" #= hd (v "b")],
--     bl "tail_and_head_c" ["tail_c" #= tl (v "c"), "head_c" #= hd (v "tail_a")]]

abStatic :: Constant
abStatic = ListC [ListC [ExprC $ EVar $ VarName "b", lInt [5, 6, 8, 9]]]


main :: IO ()
main = do
    let staticV = generateStaticVars maxSmall abStatic
    print staticV
    result <- eval mix (M.fromList [("program", EConstant $ ProgramC maxSmall), ("division", EConstant staticV), ("vs_0", EConstant abStatic)])
    case result of
      Left err -> putStrLn $ "Error: " ++ show err
      Right value -> do 
        print "Answer: "
        print value
