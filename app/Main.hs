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

main :: IO ()
main = do
    let staticTuring = generateStaticVars turingInterpreter (ListC [])
    let vs_0 = ListC [ListC [ExprC $ EVar $ VarName "program", ProgramC turingInterpreter],
                        ListC [ExprC $ EVar $ VarName "division", staticTuring],
                        ListC [ExprC $ EVar $ VarName "vs_0", ListC []]]
    let staticMix = generateStaticVars turingInterpreter vs_0
    result <- eval mix (M.fromList [ ("program", EConstant $ ProgramC mix)
                        , ("division", EConstant  staticMix)
                        , ("vs_0", EConstant vs_0)])
    print result