module Futamura.Spec where

import Test.Hspec
import qualified Data.Map.Strict as M

import Dsl
import Ast
import Mix
import Division
import Interpret
import TInterpreter

q :: Constant
q = ListC [
    ListC [s "if", IntC 0, s "goto", IntC 3],
    ListC [s "right"],
    ListC [s "goto", IntC 0],
    ListC [s "right"],
    ListC [s "if", IntC 0, s "goto", IntC 3],
    ListC [s "left"],
    ListC [s "write", IntC 1]]
input1 = EConstant $ lInt [1, 1, 0, 1, 0, 1]
output1 = EConstant $ lInt [1, 1, 0, 1]


specFutamura1 :: Spec
specFutamura1 = do
    describe "First Futamura Projection" $ do
        it "correctly reduce the Turing machine interpreter on the G" $ do
            let q_ =  ListC [ListC [ExprC $ EVar $ VarName "Q", q]]
            let staticV = generateStaticVars turingInterpreter q_
            
            result <- eval mix (M.fromList [ ("program", EConstant $ ProgramC turingInterpreter)
                                , ("division", EConstant staticV)
                                , ("vs_0", EConstant q_)
                                ])
            
            case result of
                Left err -> 
                    expectationFailure $ "Error during specialization: " ++ show err
                Right (EConstant (ProgramC specializedProgram)) -> do
                    result2 <- eval specializedProgram (M.fromList [("Right", input1)])
                    
                    case result2 of
                        Left err -> expectationFailure $ "Error running specialized program: " ++ show err
                        Right value -> value `shouldBe` output1
                _ ->  expectationFailure "Unexpected result type"

specFutamura2 :: Spec
specFutamura2 = do
    describe "Second Futamura Projection" $ do
        it "correctly generate compiler" $ do
            let q = ListC []
            let q_ =  ListC [ListC [ExprC $ EVar $ VarName "Q", q]]
            let staticTuring = generateStaticVars turingInterpreter q_
            let vs_0 = ListC [ListC [ExprC $ EVar $ VarName "program", ProgramC turingInterpreter],
                                ListC [ExprC $ EVar $ VarName "division", staticTuring]]
            let staticMix = generateStaticVars mix vs_0
            result <- eval mix (M.fromList [ ("program", EConstant $ ProgramC mix)
                                            , ("division", EConstant  staticMix)
                                            , ("vs_0", EConstant vs_0)])
            case result of
                Left err -> putStrLn $ "Error: " ++ show err
                Right (EConstant (ProgramC value)) -> do
                    let q = ListC [ ListC [s "if", IntC 0, s "goto", IntC 3],
                            ListC [s "right"],
                            ListC [s "goto", IntC 0],
                            ListC [s "write", IntC 1]]
                    let right = lInt [1, 1, 0, 1, 0, 1]
                    let vs_00 = ListC [ListC [ExprC $ EVar $ VarName "Q", q]]
                    result1 <- eval value (M.fromList [("vs_0", EConstant vs_00)])
                    case result1 of
                        Left err -> putStrLn $ "Error: " ++ show err
                        Right (EConstant (ProgramC value1)) -> do
                            -- putStrLn $ "Program1 : " ++ prettyPrintProgram value1 
                            result2 <- eval value1 (M.fromList [("Right", EConstant right)])
                            case result2 of
                                Left err -> expectationFailure $ "Error running specialized program: " ++ show err
                                Right value_tr -> value_tr `shouldBe` output1
                            
    
