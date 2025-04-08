{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Interpret where

import Control.Monad.State (StateT, put, MonadState(get), MonadTrans(lift), evalStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (traceM, trace)
import Data.List(nub, find)
import qualified Data.Map.Strict as M
import System.IO.Unsafe (unsafePerformIO)

import Ast
import Data.Type.Equality (TestEquality)
import Dsl (program)
import InterpretOp

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
        Just basicBlocks ->

            evalBasicBlocks basicBlocks
        Nothing -> lift $ throwE $ UnexpectedElement ("Expected a label named '" ++ labelName ++ "', but it was not found in the LabelMap.")

handleJumpBlock :: BasicBlock -> EvalM Constant
handleJumpBlock (BasicBlock _ _ instr@(Goto goLabel)) =
    -- trace ("goto :" ++ show goLabel)
    handleLabel goLabel instr
handleJumpBlock (BasicBlock _ _ instr@(If curExpr trueLabel falseLabel)) = do
    val <- evalExpr curExpr
    -- traceM ("IF block: " ++ show val ++ " true-Label: " ++ show trueLabel ++ " false-Label: " ++ show falseLabel)
    case val of
        IntC 1 -> handleLabel trueLabel instr
        BoolC true -> handleLabel trueLabel instr
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
    traceM ("Varname: " ++ varName ++ " = " ++ show result)
    (currentVarMap, currentLabelMap) <- get
    let updatedVarMap = M.insert varName result currentVarMap
    put (updatedVarMap, currentLabelMap)
    evalAssigments assigmentTail
evalAssigments [] = return ()

evalExpr :: Expr -> EvalM Constant
evalExpr expr = do
    reducedExpr <- reduceExpr expr
    case reducedExpr of
        EConstant c -> return c
        smth           -> error ("Evaluation failed: Expression did not reduce to a constant: Reduced expression is: " ++ show smth)


eval ::  Program -> VarMap -> IO (Either Error Constant)
eval program varInit = runExceptT (evalStateT (evalVarMap program varInit) (M.empty, M.empty))

reduceExpr :: Expr -> EvalM Expr
reduceExpr (EConstant constant) = return $ EConstant constant
reduceExpr v@(EVar (VarName varName)) = do
    (currentVarMap, _) <- get
    case M.lookup varName currentVarMap of
        Just (ExprC l) ->
                -- trace "1111111111"
                reduceExpr l
        Just element   ->
            -- trace "22222222222"
            return $ EConstant element
        Nothing        ->
            -- trace "3333333333"
            return v
reduceExpr (EBinOP op expr1 expr2) = do
    (currentVarMap, _) <- get
    leftEl <- reduceExpr expr1
    rightEl <- reduceExpr expr2
    traceM ("current binary function: " ++ show op ++ "leftEl: " ++ show leftEl ++ "  rightEl: " ++ show rightEl)
    case op of
        Reduce -> return$ reduceOp leftEl expr2
        _ -> applyBinOp leftEl rightEl op
reduceExpr (EUnOp op expr) = do
    res <- reduceExpr expr
    applyUnOp res op
reduceExpr (ETernOp op exp1 exp2 exp3) = do
    firstEl <- reduceExpr exp1
    secEl <- reduceExpr exp2
    thirdEl <- reduceExpr exp3
    applyTernOp firstEl secEl thirdEl op

getUnOpFunc :: UnOp -> (Constant -> Constant)
getUnOpFunc op = case op of
    Hd -> headOp
    Tl -> tailOp

getBinOpFunc :: BinOp -> (Constant -> Constant -> Constant)
getBinOpFunc op = case op of
    Plus      -> plus
    Equal     -> equal
    DropWhile -> dropWhileOp
    Drop      -> dropOp
    Union     -> unionOp
    Lookup    -> lookupOp
    Elem      -> elemOp
    Eval      -> evalOp
    Cons      -> consOp

getTernOpFunc :: TernOp -> (Constant -> Constant -> Constant -> Constant)
getTernOpFunc op = case op of
    Insert -> insertOp


applyUnOp :: Expr -> UnOp -> EvalM Expr
applyUnOp expr1 op = do
    case expr1 of
        (EConstant c1) -> return $ EConstant (getUnOpFunc op c1)
        _                            -> return $ EUnOp op expr1


applyBinOp :: Expr -> Expr -> BinOp  -> EvalM Expr
applyBinOp expr1 expr2 op = do
    case (expr1, expr2) of
        (EConstant c1, EConstant c2) -> return $ EConstant (getBinOpFunc op c1 c2)
        (EVar v, EConstant c2)       -> return $ EConstant (getBinOpFunc op (ExprC expr1) c2)
        _                            -> return $ EBinOP op expr1 expr2


applyTernOp :: Expr -> Expr -> Expr -> TernOp -> EvalM Expr
applyTernOp expr1 expr2 expr3 op = do
    case (expr1, expr2, expr3) of
        (EVar c1, EConstant c2, EConstant c3) -> return $ EConstant (getTernOpFunc op (ExprC expr1) c2 c3)
        _                                           -> return $ ETernOp op expr1 expr2 expr3


reduceOp :: Expr -> Expr -> Expr
reduceOp expr (EConstant (ListC constants)) = do
    let result = unsafePerformIO $ runExceptT $ evalStateT (reduceExpr expr) (varListToMap constants, M.empty)
    case result of
        Left e -> undefined
        Right value -> value

evalOp :: Constant -> Constant -> Constant
evalOp (ExprC expr) (ListC constants) = do
    let result = unsafePerformIO $ runExceptT $ evalStateT (evalExpr expr) (varListToMap constants, M.empty)
    case result of
        Left e -> undefined
        Right value ->
            -- trace ("changed Map: " ++ show changedMap)
            value
evalOp e1 e2 = do
    -- trace ("eval op: " ++ show e1 ++ "  _:_   " ++ show e2)
            undefined

