{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Interpret where

import Control.Monad.State (StateT, put, MonadState(get), MonadTrans(lift), evalStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (traceM, trace)
import qualified Data.Map.Strict as M
import Data.List(nub, find)

import Ast
import Data.Type.Equality (TestEquality)
import Dsl (program)

data Error = UndefinedVar String | VariableNotFound String | UnexpectedElement String deriving (Show)

type VarMap = M.Map String Constant
type LabelMap = M.Map String [BasicBlock]

type EvalM = StateT (VarMap, LabelMap) (ExceptT Error IO)

evalVarMap :: Program -> VarMap -> EvalM Constant
evalVarMap (Program ((VarName varName) : varTail) basicBlocks) varMap = do
    -- liftIO $ traceM "evalVarMap"
    case M.lookup varName varMap of
        Just val -> do
            -- traceM ("Curren var here : " ++ show varName ++ " " ++ show val)
            (currentVarMap, currentLabelMap) <- get
            let updatedState = M.insert varName val currentVarMap
            put (updatedState, currentLabelMap)
            evalVarMap (Program varTail basicBlocks) varMap
        Nothing -> lift $ throwE $ VariableNotFound varName
evalVarMap (Program [] basicBlocks) _ = do
    let labelMap = evalLabels basicBlocks
    (currentVarMap, _) <- get
    put (currentVarMap, labelMap)
    evalBasicBlocks basicBlocks

evalLabels :: [BasicBlock] -> LabelMap
evalLabels (curBlock@(BasicBlock (Label label) _ _) : tailBlocks) =
    -- trace ("LABELS INSERT: " ++ label ++ " ")
    M.insert label (curBlock : tailBlocks) (evalLabels tailBlocks)
evalLabels ((BasicBlock EmptyLabel _ _) : tailBlocks) = evalLabels tailBlocks
evalLabels [] = M.empty


handleLabel :: Label -> Jump -> EvalM Constant
handleLabel EmptyLabel instr  =
    lift $ throwE $ UnexpectedElement ("Expected a non-empty label, but got EmptyLabel in '" ++ show instr ++ "'instruction.")
handleLabel (Label labelName) _ = do
    (_, currentLabelMap) <- get
    case M.lookup labelName currentLabelMap of
        Just basicBlocks -> evalBasicBlocks basicBlocks
        Nothing -> lift $ throwE $ UnexpectedElement ("Expected a label named '" ++ labelName ++ "', but it was not found in the LabelMap.")

handleJumpBlock :: BasicBlock -> EvalM Constant
handleJumpBlock (BasicBlock _ _ instr@(Goto goLabel)) = handleLabel goLabel instr
handleJumpBlock (BasicBlock _ _ instr@(If curExpr trueLabel falseLabel)) = do
    val <- evalExpr curExpr
    -- traceM ("IF block: " ++ show val ++ " truelabel: " ++ show trueLabel ++ "false Label: " ++ show falseLabel)
    case val of
        IntC 1 -> handleLabel trueLabel instr
        _ -> handleLabel falseLabel instr
handleJumpBlock (BasicBlock _ _ (Return curExpr)) = evalExpr curExpr
handleJumpBlock _ = lift $ throwE $ UnexpectedElement "In jump blocks"

evalBasicBlocks :: [BasicBlock] -> EvalM Constant
evalBasicBlocks (BasicBlock _ assigments EmptyJump : tailBlocks) = do
    -- traceM "evalBasicBlocks empty jump"
    evalAssigments assigments
    evalBasicBlocks tailBlocks
evalBasicBlocks (block@(BasicBlock _ assigments _) : _) = do
    -- traceM "evalBasicBlocks jump"
    evalAssigments assigments
    handleJumpBlock block
evalBasicBlocks [] =  lift $ throwE $ UnexpectedElement "Missing return value in any block"

evalAssigments :: [Assigment] -> EvalM ()
evalAssigments (Assigment (VarName varName) expr1 : assigmentTail) = do
    result <- evalExpr expr1
    -- traceM ("Varname: " ++ varName ++ " result: " ++ show result)
    (currentVarMap, currentLabelMap) <- get
    let updatedVarMap = M.insert varName result currentVarMap
    put (updatedVarMap, currentLabelMap)
    evalAssigments assigmentTail
evalAssigments [] = return ()

evalExpr :: Expr -> EvalM Constant
evalExpr (EConstant constant) = return constant
evalExpr (EVar (VarName varName)) = do
    (currentVarMap, _) <- get
    case M.lookup varName currentVarMap of
        Just element -> return element
        Nothing -> lift $ throwE $ VariableNotFound varName
evalExpr (EBinOP op expr1 expr2) = do
    leftEl <- evalExpr expr1
    rightEl <- evalExpr expr2
    -- traceM ("Current bin operation: " ++ show op ++ " " ++ show leftEl ++ "  " ++ show rightEl)
    case op of
        Plus -> return $ plus leftEl rightEl
        Equal -> return $ equal leftEl rightEl
        DropWhile -> return $ dropWhileOp leftEl rightEl
        Drop -> return $ dropOp leftEl rightEl
        Union -> return $ unionOp leftEl rightEl
        Lookup -> return $ lookupOp leftEl rightEl
evalExpr (EUnOp op expr) = do
    res <- evalExpr expr
    case op of
        Hd -> return $ headOp res
        Tl -> return $ tailOp res

equal :: Constant -> Constant -> Constant
equal x y = if x == y then IntC 1 else IntC 0

dropWhileOp :: Constant -> Constant -> Constant
dropWhileOp a (ListC b) = ListC $ dropWhile (/= a) b

dropOp :: Constant -> Constant -> Constant
dropOp (IntC a) (ListC b) = ListC $ drop a b

plus :: Constant -> Constant -> Constant
plus (IntC intLeft) (IntC intRight) = IntC $ intLeft + intRight
plus (ListC listLeft) (ListC listRight) = ListC $ listLeft ++ listRight
plus const (ListC listRight) = ListC $ const : listRight
plus (StrC strLeft) (StrC strRight) = StrC $ strLeft ++ strRight

headOp :: Constant -> Constant
headOp (ListC (a : aTail)) = a
headOp _ = undefined

tailOp :: Constant -> Constant
tailOp (ListC (a : aTail)) = ListC aTail
tailOp (StrC (a : aTail)) = StrC aTail

unionOp :: Constant -> Constant -> Constant
unionOp (ListC leftList) (ListC rightList) = ListC (nub (leftList ++ rightList))
unionOp _ _ = undefined

eval ::  Program -> VarMap -> IO (Either Error Constant)
eval program varInit = runExceptT (evalStateT (evalVarMap program varInit) (M.empty, M.empty))

lookupOp :: Constant -> Constant -> Constant
lookupOp (ProgramC (Program _ basicBlocks)) (StrC label)=
    let labeledBlock = find (\(BasicBlock l _ _) -> l == Label label) basicBlocks
    in
    case labeledBlock of
        Just block -> blockToCommandsList block
        Nothing -> error $ "Block with label: " ++ label ++ " not found."

-- First element in list will be indentificator: assigment, goto, if, return
blockToCommandsList :: BasicBlock -> Constant
blockToCommandsList (BasicBlock label assigments jump) =
    let assigmentList = map (\(Assigment varName expr) -> ListC [StrC "assigment", ExprC $ EVar varName , ExprC expr]) assigments
        jumpElement = jumpToCommand jump
    in if jumpElement == []
       then ListC assigmentList
       else ListC $ assigmentList ++ [ListC jumpElement]


jumpToCommand :: Jump -> [Constant]
jumpToCommand (Goto (Label labelName)) = [StrC "goto", StrC labelName]
jumpToCommand (If expr1 (Label ifTrueLabel) (Label ifFalseLabel)) = [StrC "if", ExprC expr1, StrC ifTrueLabel, StrC ifFalseLabel]
jumpToCommand (Return expr) = [StrC "return", ExprC expr]
jumpToCommand EmptyJump = []