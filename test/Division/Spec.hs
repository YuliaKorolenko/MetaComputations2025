module Division.Spec where

import Test.Hspec
import Data.List (sort)

import Division
import Dsl
import Ast

testProgramXY = program ["k", "n"]
            [block ["x" #= (4 :: Int)],
            block ["f" #= "k"],
            bja ["y" #= (6 :: Int)] $ returnCnst "x",
            block ["f" #= "n"]]

testABC = program ["a", "b", "c"]
            [blja "l1" ["d" #= pl (v "a") (v "b")] (if' (Constant $ IntConst 1) "l2" "l1"),
             blja "l2" ["a" #= v "c"] (if' (Constant $ IntConst 1) "l1" "l2")]

specDivision :: Spec
specDivision = do 
    describe "Test Program testProgramXY" $ do
        it "correctly determine all program variables" $ do
            let result = allProgramVars testProgramXY
            sort result `shouldBe` (sort [VarName "x", VarName "y", VarName "k", VarName "f", VarName "n"])
    describe "Test Program testProgramXY" $ do
        it "correctly generate static variables" $ do
            let result = generateStaticVars testProgramXY [VarName "k"]
            sort result `shouldBe` (sort [VarName "x", VarName "y", VarName "k"])
    describe "Test Program testProgramXY" $ do
        it "correctly generate static variables" $ do
            let result = generateStaticVars testABC [VarName "a", VarName "b"]
            sort result `shouldBe` (sort [VarName "b"])

