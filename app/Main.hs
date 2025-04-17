module Main where

import qualified Data.Map.Strict as M

import InterpretOp
import Dsl
import TInterpreter
import Division
import System.IO
import Interpret 
import Mix
import Prelude 
import Ast 


maxSmall :: Program
maxSmall = program ["a", "b", "c"]
    [blja "initial" [ "tail_a" #= tl (v "b"), "head_a" #= hd (v "a")] (goto "tail_and_head_b"),
     blja "tail_and_head_b" ["tail_b" #= tl (v "b"), "head_b" #= hd (v "b")] (Return (v "tail_a"))]

dictSearchProgram :: Program
dictSearchProgram = program ["name", "namelist", "valuelist"]
    [ blj "initial" (if' (v "name" ?= hd (v "namelist")) "found"  "cont"), 
      blja "cont" [ 
        "valuelist" #= tl (v "valuelist"), 
        "namelist" #= tl (v "namelist")] (goto "initial"),
      blj "found" (Return (hd (v "valuelist")))
    ]

dictSearchStatic :: Constant
dictSearchStatic =  ListC [ListC [ExprC $ EVar $ VarName "name", s "Alice"],
                           ListC [ExprC $ EVar $ VarName "namelist", lStr ["Bob", "Charlie", "Alice"]]]

maxProgramWithConditions :: Program
maxProgramWithConditions = program ["a", "b", "c"]
    [blja "initial" ["a" #= tl (v "b")] (goto "tail_and_head_a"),
    blja "tail_and_head_a" [ "tail_a" #= tl (v "a"), "head_a" #= hd (v "a")] (goto "tail_and_head_b"),
    blja "tail_and_head_b" ["tail_b" #= tl (v "b"), "head_b" #= hd (v "b")] (goto "tail_and_head_c"),
    blja "tail_and_head_c" ["tail_c" #= tl (v "c"), "head_c" #= hd (v "tail_a")] (Return (pl (v "tail_a") (v "tail_c")))]

maxProgramWithConditionsResult :: Program
maxProgramWithConditionsResult = program []
    [blja "initial" ["tail_c" #= tl (v "c")] (Return (pl (EConstant $ lInt [8, 9]) (v "tail_c")))]

abStatic :: Constant
abStatic = ListC [ListC [ExprC $ EVar $ VarName "a", lInt [1, 2, 3]],
                  ListC [ExprC $ EVar $ VarName "b", lInt [5, 6, 8, 9]]]

dictSearchDynamicIf :: Constant
dictSearchDynamicIf =  ListC [ListC [ExprC $ EVar $ VarName "name", s "Alice"]]

main :: IO ()
main = do
    let staticV = generateStaticVars dictSearchProgram dictSearchDynamicIf
    result <- eval mix (M.fromList [ ("program", EConstant $ ProgramC dictSearchProgram)
                                    , ("division", EConstant staticV)
                                    , ("vs_0", EConstant dictSearchDynamicIf)
                                    ])
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (EConstant (ProgramC value)) -> print ("AAAAAAAA:" ++ show value)
                            