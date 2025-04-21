module MixHelpers.Spec where

import Test.Hspec
import Data.List (sort)

import InterpretOp(lookupOp, headOp, tailOp, elemOp, insertOp, blockToCommandsList, commandsListToBlock, toProgramOp, checkAllVars)
import Interpret(reduceOp, eval)
import Division
import Dsl
import Ast
import Test.Tasty.Runners (Outcome(Failure))
import qualified Data.Map.Strict as M
import InterpretOp (findByLabelOp)
import TInterpreter

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

right :: Constant
right =  ListC [ListC [ExprC $ EVar $ VarName "Right", lInt [1, 1, 0, 1, 0, 1]]]

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
        it "correctly generate static variables for turing interpreter" $ do
            let result = generateStaticVars turingInterpreter right
            resultToVarNames result `shouldBe` []


answer1 :: Constant
answer1 = ListC [
    s "l2",
    ListC [s "assigment", ExprC $ v "a", ExprC $ v "c"],
    ListC [s "if", ExprC $ EConstant $ IntC 1, s "l1", s "l2"]]

answer2 :: Constant
answer2 = ListC [
    s "check",
    ListC [s "assigment", ExprC $ v "y", ExprC $ EConstant $ IntC 6],
    ListC [s "assigment", ExprC $ v "l", ExprC $ v "n"],
    ListC [s "return", ExprC $ v "x"]]

specLookup :: Spec
specLookup = do
    describe "Test lookup on testABC" $ do
        it "correctly create list of command in basicBlock answer1" $ do
            let result = lookupOp (ProgramC testABC) (s "l2")
            result `shouldBe` answer1
    describe "Test lookup on testProgramXY" $ do
        it "correctly create list of command in basicBlock answer2" $ do
            let result = lookupOp (ProgramC testProgramXY) (s "check")
            result `shouldBe` answer2
    describe "Test lookup on variable and var storage" $ do
        it "correctly find variable in storage" $ do
            let varnameToFind = ExprC (EVar (VarName "a"))
            let env = abStatic
            let result = lookupOp env varnameToFind
            result `shouldBe` IntC 1

specIsStatic :: Spec
specIsStatic = do
    describe "Test checkAllVars on testABC" $ do
        it "correctly not find element in static variables" $ do
            let staticVariables = generateStaticVars testABC abStatic
            let bb = lookupOp (ProgramC testABC) (s "l1")
            let command = headOp $ tailOp $ tailOp bb
            print ("Command in Test checkAllVars on testABC" ++ show command)
            let x = headOp $ tailOp command
            print ("Test checkAllVars on testABC 123: " ++ show x)
            let trueFalse = checkAllVars x staticVariables
            trueFalse `shouldBe` BoolC True
    describe "Test checkAllVars on testProgramXY" $ do
        it "correctly find element in static variables" $ do
            let staticVariables = generateStaticVars testProgramXY xyStatic
            let bb = lookupOp (ProgramC testProgramXY) (s "check")
            let command = headOp $ tailOp bb
            let x = headOp $ tailOp command
            let trueFalse = checkAllVars x staticVariables
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

programWithReduced :: Program
programWithReduced = program ["vs_0", "list_a"] [bja ["a_" #= hd (v "list_a"),
                                                      "reduced_a" #= reduce' (v "a_") (v "vs_0")]
                                                (Return (v "reduced_a"))]

aVS_0 :: Constant
aVS_0  =  ListC [ListC [ExprC $ EVar $ VarName "a", s "Alice"]]

specReduce :: Spec
specReduce = do
    describe "Test reduce' function" $ do
        it "correctly reduces expressions with constants" $ do
            result <- eval programWithReduced (M.fromList [("vs_0", EConstant aVS_0), ("list_a", EConstant $ ListC [ExprC $ v "a"])])
            case result of
                Right (EConstant (ExprC value)) -> value `shouldBe` EConstant (s "Alice")
                Left err -> putStrLn $ "Error: " ++ show err

basicBlock1 :: BasicBlock
basicBlock1 = blja "check" ["y" #= (6 :: Int), "l" #= v "n"] $ returnCnst "x"


specToFromPrgrm = describe "blockToCommandsList and commandsListToBlock roundtrip" $ do
    it "correctly converts basicBlock1 to Constant and back" $ do
        let reversedBasicBlock = commandsListToBlock $ blockToCommandsList basicBlock1
        basicBlock1 `shouldBe` reversedBasicBlock
    it "should correctly convert back to Constant format" $ do
        let blockProgram = ProgramC $ program [] [blj "tail_and_head_b" (Return (EConstant (ListC [IntC 6, IntC 8, IntC 9])))]
        let staticV = [s "tail_and_head_b"]

        let expectedConstant = ListC [ ListC [
                        StrC "tail_and_head_b",
                        ListC [StrC "return", ListC [IntC 6, IntC 8, IntC 9]]]]
        blockProgram `shouldBe` toProgramOp expectedConstant (ListC staticV) blockProgram

specList :: Spec
specList =  describe "List Operations" $ do
    it "creates empty list" $ do
        result <- eval (program [] [bja ["emptyList" #= list' []] (Return (v "emptyList"))]) M.empty
        case result of
            Left err -> putStrLn $ "Error: " ++ show err
            Right value -> value `shouldBe` EConstant (ListC [])
    it "handles mixed constants and expressions" $ do
        result <- eval
                    (program ["a", "b"] [bja ["multiList" #= list' [EConstant $ s "assigment", v "a", v "b"]] (Return (v "multiList"))])
                    (M.fromList [ ("a", EConstant $ IntC 1)
                                , ("b", EConstant $ IntC 2)])
        case result of
            Left err -> putStrLn $ "Error: " ++ show err
            Right value -> value `shouldBe` EConstant (ListC [StrC "assigment", IntC 1, IntC 2])
    it "handles pair" $ do
        result <- eval
            (program ["vs_0", "b"] [bja ["pair" #= pair (EConstant $ s "initial") (v "vs_0")] (Return (v "pair"))])
            (M.fromList [ ("vs_0", EConstant $ lInt [1, 2, 3])
                        , ("b", EConstant $ IntC 2)])
        case result of
            Left err -> putStrLn $ "Error: " ++ show err
            Right value -> value `shouldBe` EConstant (ListC [StrC "initial", lInt [1, 2, 3]])


specElem :: Spec
specElem = do
    describe "Test elemOp" $ do
        it "finds an element in a simple list" $ do
            let list = ListC [StrC "found", StrC "missing", StrC "target"]
            elemOp (StrC "target") list `shouldBe` BoolC True
            elemOp (StrC "not here") list `shouldBe` BoolC False

        it "finds an element in nested lists" $ do
            let nestedList = ListC [
                    ListC [StrC "initial", ListC [StrC "nested", StrC "Alice"]],
                    ListC [StrC "found", ListC [StrC "target", StrC "Alice"]],
                    ListC [StrC "cont", ListC [StrC "other", StrC "Bob"]]]
            findByLabelOp (StrC "initial") nestedList `shouldBe` BoolC True
            findByLabelOp (StrC "missing") nestedList `shouldBe` BoolC False

        it "handles the complex example from your binary function" $ do
            let leftEl = ListC [StrC "found", ListC [ListC [ExprC (EVar (VarName "name")), StrC "Alice"]]]
            let rightEl = ListC [
                    ListC [StrC "initial", ListC [ListC [ExprC (EVar (VarName "name")), StrC "Alice"]]],
                    ListC [StrC "found", ListC [ListC [ExprC (EVar (VarName "name")), StrC "Alice"]]],
                    ListC [StrC "cont", ListC [ListC [ExprC (EVar (VarName "name")), StrC "Alice"]]]]
            elemOp leftEl rightEl `shouldBe` BoolC True