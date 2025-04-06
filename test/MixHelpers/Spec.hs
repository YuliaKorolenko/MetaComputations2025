module MixHelpers.Spec where

import Test.Hspec
import Data.List (sort)

import Interpret(lookupOp, headOp, tailOp, elemOp)
import Division
import Dsl
import Ast
import Test.Tasty.Runners (Outcome(Failure))

testProgramXY = program ["k", "n"]
            [block ["x" #= (4 :: Int)],
            block ["f" #= "k"],
            blja "check" ["y" #= (6 :: Int), "l" #= v "n"] $ returnCnst "x",
            block ["f" #= "n"]]

testABC = program ["a", "b", "c"]
            [blja "l1" ["d" #= pl (v "a") (v "b")] (if' (EConstant $ IntC 1) "l2" "l1"),
             blja "l2" ["a" #= v "c"] (if' (EConstant $ IntC 1) "l1" "l2")]

resultToVarNames :: Constant -> [VarName]
resultToVarNames (ListC arr) = sort $ map (\(ExprC (EVar varname)) -> varname) arr

specDivision :: Spec
specDivision = do
    describe "Test allPrograms function testProgramXY" $ do
        it "correctly determine all program variables" $ do
            let result = allProgramVars testProgramXY
            sort result `shouldBe` sort [VarName "x", VarName "y", VarName "k", VarName "f", VarName "n", VarName "l"]
    describe "Test Program testProgramXY" $ do
        it "correctly generate static variables" $ do
            let result = generateStaticVars testProgramXY [VarName "k"]
            resultToVarNames result `shouldBe` sort [VarName "x", VarName "y", VarName "k"]
    describe "Test Program testProgramXY" $ do
        it "correctly generate static variables" $ do
            let result = generateStaticVars testABC [VarName "a", VarName "b"]
            resultToVarNames result `shouldBe` sort [VarName "b"]


answer1 :: Constant
answer1 = ListC [
    ListC [s "assigment", ExprC $ v "a", ExprC $ v "c"],
    ListC [s "if", ExprC $ EConstant $ IntC 1, s "l1", s "l2"]]

answer2 :: Constant
answer2 = ListC [
    ListC [s "assigment", ExprC $ v "y", ExprC $ EConstant $ IntC 6],
    ListC [s "assigment", ExprC $ v "l", ExprC $ v "n"],
    ListC [s "return", ExprC $ v "x"]]

specLookup :: Spec
specLookup = do
    describe "Test lookup on testABC" $ do
        it "correctly create list of command in basicBlock" $ do
            let result = lookupOp (ProgramC testABC) (s "l2")
            result `shouldBe` answer1
    describe "Test lookup on testProgramXY" $ do
        it "correctly create list of command in basicBlock" $ do
            let result = lookupOp (ProgramC testProgramXY) (s "check")
            result `shouldBe` answer2

specElem :: Spec
specElem = do
    describe "Test elem on testABC" $ do
        it "correctly not find element in static variables" $ do
            let staticVariables = generateStaticVars testABC [VarName "a", VarName "b"]
            let bb = lookupOp (ProgramC testABC) (s "l1")
            let command = headOp bb
            let x = headOp $ tailOp command
            let trueFalse = elemOp x staticVariables
            trueFalse `shouldBe` BoolC False
    describe "Test elem on testProgramXY" $ do
        it "correctly find element in static variables" $ do
            let staticVariables = generateStaticVars testProgramXY [VarName "k"]
            let bb = lookupOp (ProgramC testProgramXY) (s "check")
            let command = headOp bb
            let x = headOp $ tailOp command
            let trueFalse = elemOp x staticVariables
            trueFalse `shouldBe` BoolC True
