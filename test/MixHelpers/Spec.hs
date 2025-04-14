module MixHelpers.Spec where

import Test.Hspec
import Data.List (sort)

import InterpretOp(lookupOp, headOp, tailOp, elemOp, insertOp)
import Interpret(reduceOp)
import Division
import Dsl
import Ast
import Test.Tasty.Runners (Outcome(Failure))

testProgramXY = program ["k", "n"]
            [bja ["x" #= (4 :: Int)] (goto "f"),
             blja "f1" ["f" #= "k"] (goto "check"),
             blja "check" ["y" #= (6 :: Int), "l" #= v "n"] $ returnCnst "x",
             bja ["f" #= "n"] (goto "f")]

xyStatic :: Constant
xyStatic = ListC [ListC [ExprC $ EVar $ VarName "k", IntC 1]]

testABC = program ["a", "b", "c"]
            [blja "l1" ["d" #= pl (v "a") (v "b")] (if' (EConstant $ IntC 1) "l2" "l1"),
             blja "l2" ["a" #= v "c"] (if' (EConstant $ IntC 1) "l1" "l2")]

abStatic :: Constant
abStatic = ListC [ListC [ExprC $ EVar $ VarName "a", IntC 1],
                  ListC [ExprC $ EVar $ VarName "b", IntC 2]]

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
            let result = generateStaticVars testProgramXY xyStatic
            resultToVarNames result `shouldBe` sort [VarName "x", VarName "y", VarName "k"]
    describe "Test Program testProgramXY" $ do
        it "correctly generate static variables" $ do
            let result = generateStaticVars testABC abStatic
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
    describe "Test lookup on variable and var storage" $ do
        it "correctly find variable in storage" $ do
            let varnameToFind = ExprC (EVar (VarName "a"))
            let env = abStatic
            let result = lookupOp env varnameToFind
            result `shouldBe` IntC 1

specElem :: Spec
specElem = do
    describe "Test elem on testABC" $ do
        it "correctly not find element in static variables" $ do
            let staticVariables = generateStaticVars testABC abStatic
            let bb = lookupOp (ProgramC testABC) (s "l1")
            let command = headOp bb
            let x = headOp $ tailOp command
            let trueFalse = elemOp x staticVariables
            trueFalse `shouldBe` BoolC False
    describe "Test elem on testProgramXY" $ do
        it "correctly find element in static variables" $ do
            let staticVariables = generateStaticVars testProgramXY xyStatic
            let bb = lookupOp (ProgramC testProgramXY) (s "check")
            let command = headOp bb
            let x = headOp $ tailOp command
            let trueFalse = elemOp x staticVariables
            trueFalse `shouldBe` BoolC True

specInsert :: Spec
specInsert = do
    describe "Test insertOp" $ do
        it "correctly inserts a new element" $ do
            let varnames = ListC [
                    ListC [ExprC (EVar (VarName "a")), IntC 1],
                    ListC [ExprC (EVar (VarName "b")), IntC 2]
                  ]
            let varToFind = ExprC (EVar (VarName "c"))  
            let res = IntC 3  
            let updatedList = insertOp varToFind res varnames
            let expectedList = ListC [
                    ListC [ExprC (EVar (VarName "a")), IntC 1],
                    ListC [ExprC (EVar (VarName "b")), IntC 2],
                    ListC [ExprC (EVar (VarName "c")), IntC 3] 
                  ]
            updatedList `shouldBe` expectedList

        it "correctly replaces an existing element" $ do
            let varnames = ListC [
                    ListC [ExprC (EVar (VarName "a")), IntC 1],
                    ListC [ExprC (EVar (VarName "b")), IntC 2]
                  ]
            let varToFind = ExprC (EVar (VarName "b"))  
            let res = IntC 3 
            let updatedList = insertOp varToFind res varnames
            let expectedList = ListC [
                    ListC [ExprC (EVar (VarName "a")), IntC 1],
                    ListC [ExprC (EVar (VarName "b")), IntC 3]  
                  ]
            updatedList `shouldBe` expectedList


specReduce :: Spec
specReduce = do
    describe "Test reduce' function" $ do
        it "correctly reduces when one variable is not in the list" $ do
            let expr = EBinOP Plus (v "a") (v "b")
            let varnames = EConstant (ListC [ListC [ExprC (EVar (VarName "a")), IntC 1]])
            let reducedExpr = reduceOp expr varnames
            let expectedExpr = EBinOP Plus (EConstant $ IntC 1) (v "b")
            reducedExpr `shouldBe` expectedExpr
        it "correctly handles expressions with constants" $ do
            let expr = EBinOP Plus (v "a") (EConstant $ IntC 2)  
            let varnames = EConstant $ ListC [ListC [ExprC (v "a"), IntC 1]]
            let reducedExpr = reduceOp expr varnames
            let expectedExpr = EConstant $ IntC 3 
            reducedExpr `shouldBe` expectedExpr

