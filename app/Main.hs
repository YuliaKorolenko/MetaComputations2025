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
import PrettyPrint
main :: IO ()
main = do
    -- putStrLn $ prettyPrintConstant $ ProgramC mix
    -- putStrLn $ prettyPrintConstant $ ProgramC turingInterpreter
    let q = ListC []
    let q_ =  ListC [ListC [ExprC $ EVar $ VarName "Q", q]]
    let staticTuring = generateStaticVars turingInterpreter q_
    let vs_0 = ListC [ListC [ExprC $ EVar $ VarName "program", ProgramC turingInterpreter],
                        ListC [ExprC $ EVar $ VarName "division", staticTuring]]
    let staticMix = generateStaticVars mix vs_0
    -- print $ prettyPrintConstant staticMix
    result <- eval mix (M.fromList [ ("program", EConstant $ ProgramC mix)
                                    , ("division", EConstant  staticMix)
                                    , ("vs_0", EConstant vs_0)])
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (EConstant (ProgramC value)) -> 
            putStrLn $ "Result: " ++ prettyPrintProgram value 