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

abStatic :: Constant
abStatic = ListC [ListC [ExprC $ EVar $ VarName "b", lInt [5, 6, 8, 9]]]

-- basicBlock1 :: BasicBlock
-- basicBlock1 = blja "check" ["y" #= (6 :: Int), "l" #= v "n"] $ returnCnst "x"

main :: IO ()
main = do
      let staticV = generateStaticVars dictSearchProgram dictSearchStatic
      result <- eval mix (M.fromList [ ("program", EConstant (ProgramC dictSearchProgram))
                                      , ("division", EConstant staticV)
                                      , ("vs_0", EConstant dictSearchStatic)
                                      ])
      print result
                            