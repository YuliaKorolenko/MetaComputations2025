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
import Ast 
import Control.Arrow (ArrowChoice(right))
import Ast (VarName(VarName), Constant (ListC))
main :: IO ()
main = do
    let staticMix = ListC [ExprC (EVar (VarName "program")),ExprC (EVar (VarName "division"))]
    let vs_1 = ListC [ListC [ExprC $ EVar $ VarName "program", ProgramC mix],
                    ListC [ExprC $ EVar $ VarName "division", staticMix]]
    
    result <- eval mix (M.fromList [ ("program", EConstant $ ProgramC mix)
                                , ("division", EConstant staticMix)
                                , ("vs_0", EConstant vs_1)])
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (EConstant (ProgramC value)) -> do
            putStrLn $ "Program1 : " ++ prettyPrintProgram value 

            let q_ =  ListC [ListC [ExprC $ EVar $ VarName "Q", ListC []]]
            let staticTuring = generateStaticVars turingInterpreter q_ 
            let vs_00 = ListC [ListC [ExprC $ EVar $ VarName "program", ProgramC turingInterpreter,
                                      ExprC $ EVar $ VarName "division", staticTuring]]
            result1 <- eval value (M.fromList [("vs_0", EConstant vs_00)])
            case result1 of 
                Left err -> putStrLn $ "Error: " ++ show err
                Right (EConstant (ProgramC value1)) -> do
                    putStrLn $ "Program2 : " ++ prettyPrintProgram value1 
                    let q = ListC [ ListC [s "if", IntC 0, s "goto", IntC 3],
                                ListC [s "right"],
                                ListC [s "goto", IntC 0],
                                ListC [s "write", IntC 1]]
                    let vs_01 = ListC [ListC [ExprC $ EVar $ VarName "Q", q]]
                    result2 <- eval value1 (M.fromList [("vs_0", EConstant vs_01)])
                    case result2 of 
                        Left err -> putStrLn $ "Error: " ++ show err
                        Right (EConstant (ProgramC value2)) -> do
                            putStrLn $ "Program3 : " ++ prettyPrintProgram value2 


    
    