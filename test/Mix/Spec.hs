module Mix.Spec where

import Test.Hspec
import qualified Data.Map.Strict as M

import Dsl
import Ast
import Mix
import Division (generateStaticVars)
import Interpret (eval)


maxProgramWithConditions :: Program
maxProgramWithConditions = program ["a", "b", "c"]
    [blja "initial" ["a" #= tl (v "b")] (goto "tail_and_head_a"),
    blja "tail_and_head_a" [ "tail_a" #= tl (v "a"), "head_a" #= hd (v "a")] (goto "tail_and_head_b"),
    blja "tail_and_head_b" ["tail_b" #= tl (v "b"), "head_b" #= hd (v "b")] (goto "tail_and_head_c"),
    blja "tail_and_head_c" ["tail_c" #= tl (v "c"), "head_c" #= hd (v "tail_a")] (Return (pl (v "tail_a") (v "tail_c")))]

maxProgramWithConditionsResult :: Program
maxProgramWithConditionsResult = program ["c"]
    [blja "(initial, 'a'=[1,2,3]; 'b'=[5,6,8,9])" ["tail_c" #= tl (v "c")] (Return (pl (EConstant $ lInt [8, 9]) (v "tail_c")))]

abStatic :: Constant
abStatic = ListC [ListC [ExprC $ EVar $ VarName "a", lInt [1, 2, 3]],
                  ListC [ExprC $ EVar $ VarName "b", lInt [5, 6, 8, 9]]]

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

dictSearchDynamicIf :: Constant
dictSearchDynamicIf =  ListC [ListC [ExprC $ EVar $ VarName "name", s "Alice"]]

dictSearchProgramResult :: Program
dictSearchProgramResult = program ["valuelist"]
    [ blja "(initial, 'name'=Alice; 'namelist'=[Bob,Charlie,Alice])" ["valuelist" #= tl (v "valuelist"), "valuelist" #= tl (v "valuelist")] (Return (hd (v "valuelist")))]


specMix :: Spec
specMix = do
    describe "Mix example" $ do
        it "example without vars and if" $ do
            let staticV = generateStaticVars maxProgramWithConditions abStatic
            result <- eval mix (M.fromList [("program", EConstant $ ProgramC maxProgramWithConditions), ("division", EConstant staticV), ("vs_0", EConstant abStatic)])
            case result of
                Left err -> putStrLn $ "Error: " ++ show err
                Right (EConstant (ProgramC value)) -> value `shouldBe` maxProgramWithConditionsResult
                _ -> undefined
        it "should specialize the dictionary search program correctly without dynamic if" $ do
            let staticV = generateStaticVars dictSearchProgram dictSearchStatic
            result <- eval mix (M.fromList [ ("program", EConstant $ ProgramC dictSearchProgram)
                                           , ("division", EConstant staticV)
                                           , ("vs_0", EConstant dictSearchStatic)
                                           ])
            case result of
                Left err -> putStrLn $ "Error: " ++ show err
                Right (EConstant (ProgramC value)) -> do value `shouldBe` dictSearchProgramResult
                _ -> undefined
        it "should specialize the dictionary search program correctly with dynamic if" $ do
            let staticV = generateStaticVars dictSearchProgram dictSearchDynamicIf
            result <- eval mix (M.fromList [ ("program", EConstant $ ProgramC dictSearchProgram)
                                           , ("division", EConstant staticV)
                                           , ("vs_0", EConstant dictSearchDynamicIf)
                                           ])
            case result of
                Left err -> putStrLn $ "Error: " ++ show err
                Right (EConstant (ProgramC prgrm)) -> do
                    result <- eval prgrm (M.fromList [ ("namelist", EConstant $ lStr ["Bob", "Alice", "Charlie"]),
                                                          ("valuelist", EConstant $ lStr ["Bobby", "Hopstone", "Fuggy"])])
                    case result of
                        Left err -> putStrLn $ "Error: " ++ show err
                        Right value -> value `shouldBe` EConstant (s "Hopstone")
                    result <- eval prgrm (M.fromList [ ("namelist", EConstant $ lStr ["Apple", "Banana", "Orange", "Alice"]),
                                        ("valuelist", EConstant $ lInt [1, 9, 12, 1456])])
                    case result of
                        Left err -> putStrLn $ "Error: " ++ show err
                        Right value -> value `shouldBe` EConstant (IntC 1456)
