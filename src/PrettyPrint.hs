module PrettyPrint where

import Ast
import Data.List (intercalate)

prettyPrintProgram :: Program -> String
prettyPrintProgram program@(Program vars blocks) =
    let prettyBlocks = intercalate "\n" (map ("    " ++) (map prettyPrintBlock blocks))
    in "Program " ++ prettyPrintVarNames vars ++ "\n[\n" ++ prettyBlocks ++ "\n]"

prettyPrintVarNames :: [VarName] -> String
prettyPrintVarNames vars = "[" ++ intercalate ", " (map show vars) ++ "]"

prettyPrintBlock :: BasicBlock -> String
prettyPrintBlock (BasicBlock label assignments jump) =
    "BasicBlock " ++ prettyPrintLabel label ++ "\n" ++
    "   Assigment: " ++
    intercalate "\n" (map ("    " ++) (map prettyPrintAssignment assignments)) ++ "\n" ++
    "   Jump: " ++ prettyPrintJump jump

prettyPrintLabel :: Label -> String
prettyPrintLabel EmptyLabel = "<empty>"
prettyPrintLabel (Label name) = 
    let second = if length name > 1 then [name !! 1] else "_"
        fourth = if length name > 3 then [name !! 3] else "_"
        lastChar =if length name > 5 then [name !! 5] else "_"
    in "Label " ++ second ++ fourth ++ lastChar


prettyPrintAssignment :: Assigment -> String
prettyPrintAssignment (Assigment var expr) =
    show var ++ " = " ++ prettyPrintExpr expr


prettyPrintJump :: Jump -> String
prettyPrintJump (Goto label) = "goto " ++ prettyPrintLabel label
prettyPrintJump (If expr trueLabel falseLabel) =
    "if " ++ prettyPrintExpr expr ++ " then " ++
    prettyPrintLabel trueLabel ++ " else " ++
    prettyPrintLabel falseLabel
prettyPrintJump (Return expr) = "return " ++ prettyPrintExpr expr


prettyPrintExpr :: Expr -> String
prettyPrintExpr (EConstant constant) = prettyPrintConstant constant
prettyPrintExpr (EVar var) = show var
prettyPrintExpr (EBinOP op expr1 expr2) =
    "(" ++ prettyPrintExpr expr1 ++ " " ++ prettyPrintBinOp op ++ " " ++ prettyPrintExpr expr2 ++ ")"
prettyPrintExpr (EUnOp op expr) =
    prettyPrintUnOp op ++ "(" ++ prettyPrintExpr expr ++ ")"
prettyPrintExpr (ETernOp op expr1 expr2 expr3) =
    prettyPrintTernOp op ++ "(" ++ prettyPrintExpr expr1 ++ ", " ++
    prettyPrintExpr expr2 ++ ", " ++ prettyPrintExpr expr3 ++ ")"


prettyPrintConstant :: Constant -> String
prettyPrintConstant (IntC n) = show n
prettyPrintConstant (BoolC b) = show b
prettyPrintConstant (StrC s) = show s
prettyPrintConstant (ListC constants) =
    "[" ++ intercalate ", " (map prettyPrintConstant constants) ++ "]"
prettyPrintConstant (ProgramC program) =
    "<Program: " ++ show (length (getBasicBlocks program)) ++ " blocks>"
prettyPrintConstant (ExprC expr) = prettyPrintExpr expr


getBasicBlocks :: Program -> [BasicBlock]
getBasicBlocks (Program _ blocks) = blocks


prettyPrintBinOp :: BinOp -> String
prettyPrintBinOp op = case op of
    Plus -> "+"
    Equal -> "=="
    NotEqual -> "!="
    Cons -> "cons"
    Drop -> "drop"
    DropWhile -> "dropWhile"
    Lookup -> "lookup"
    Eval -> "eval"
    Reduce -> "reduce"
    Union -> "union"
    IsStatic -> "isStatic"
    Elem -> "elem"


prettyPrintUnOp :: UnOp -> String
prettyPrintUnOp op = case op of
    Hd -> "hd"
    Tl -> "tl"
    GenLabel -> "genLabel"


prettyPrintTernOp :: TernOp -> String
prettyPrintTernOp op = case op of
    Insert -> "insert"
    ToPrgrm -> "toProgram"


prettyPrintProgramIndented :: Int -> Program -> String
prettyPrintProgramIndented indent (Program vars blocks) =
    replicate indent ' ' ++ "Program " ++ prettyPrintVarNames vars ++ "\n" ++
    replicate indent ' ' ++ "  Blocks:\n" ++
    intercalate "\n" (map (replicate indent ' ' ++) (map ("    " ++) (map prettyPrintBlock blocks)))


prettyPrintBlockIndented :: Int -> BasicBlock -> String
prettyPrintBlockIndented indent (BasicBlock label assignments jump) =
    replicate indent ' ' ++ prettyPrintLabel label ++ ":\n" ++
    replicate indent ' ' ++ "  Assignments:\n" ++
    intercalate "\n" (map (replicate indent ' ' ++) (map ("    " ++) (map prettyPrintAssignment assignments))) ++ "\n" ++
    replicate indent ' ' ++ "  Jump: " ++ prettyPrintJump jump 