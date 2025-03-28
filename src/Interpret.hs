{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Interpret where

import Control.Monad.State (StateT, put, MonadState(get), MonadTrans(lift), evalStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace
import qualified Data.Map.Strict as M

import Ast
import Data.Type.Equality (TestEquality)

data Error = UndefinedVar String | VariableNotFound String | UnexpectedElement String deriving (Show)

type VarMap = M.Map String Constant
type LabelMap = M.Map String [BasicBlock]

type EvalM = StateT (VarMap, LabelMap) (ExceptT Error IO)                           

evalVarMap :: Program -> VarMap -> EvalM Constant
evalVarMap (Program ((VarName varName) : varTail) basicBlocks) varMap = do
    liftIO $ traceM "evalVarMap"
    case M.lookup varName varMap of
        Just val -> do
            traceM ("Curren var : " ++ show varName ++ " " ++ show val)
            (currentVarMap, currentLabelMap) <- get 
            let updatedState = M.insert varName val currentVarMap
            put (updatedState, currentLabelMap)
            evalVarMap (Program varTail basicBlocks) varMap
        Nothing -> lift $ throwE $ VariableNotFound varName
evalVarMap (Program [] basicBlocks) _ = do
    let labelMap = evalLabels basicBlocks
    (currentVarMap, _) <- get
    put (currentVarMap, labelMap)
    -- (_, currentLabelMap1) <- get
    -- liftIO $ do
    --     putStrLn "Current LabelMap:"
    --     mapM_ (\(k, v) -> putStrLn $ k ++ " -> " ++ show v) (M.toList currentLabelMap1)
    evalBasicBlocks basicBlocks

evalLabels :: [BasicBlock] -> LabelMap
evalLabels (curBlock@(BasicBlock (Label label) _ _) : tailBlocks) = M.insert label (curBlock : tailBlocks) (evalLabels tailBlocks)
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
handleJumpBlock (BasicBlock _ _ instr@(GOTO goLabel)) = handleLabel goLabel instr
handleJumpBlock (BasicBlock _ _ instr@(IF curExpr trueLabel falseLabel)) = do
    val <- evalExpr curExpr
    case val of
        IntConst 1 -> handleLabel trueLabel instr
        _ -> handleLabel falseLabel instr
handleJumpBlock (BasicBlock _ _ (RETURN curExpr)) = evalExpr curExpr
handleJumpBlock _ = lift $ throwE $ UnexpectedElement "In jump blocks"

evalBasicBlocks :: [BasicBlock] -> EvalM Constant
evalBasicBlocks (BasicBlock _ assigments EMPTYJUMP : tailBlocks) = do
    evalAssigments assigments
    evalBasicBlocks tailBlocks
evalBasicBlocks (block@(BasicBlock _ assigments _) : _) = do
    evalAssigments assigments
    handleJumpBlock block
evalBasicBlocks [] =  lift $ throwE $ UnexpectedElement "Missing return value in any block"

evalAssigments :: [Assigment] -> EvalM ()
evalAssigments (Assigment (VarName varName) expr1 : assigmentTail) = do
    result <- evalExpr expr1
    (currentVarMap, currentLabelMap) <- get
    let updatedVarMap = M.insert varName result currentVarMap
    put (updatedVarMap, currentLabelMap)
    evalAssigments assigmentTail
evalAssigments [] = return ()

evalExpr :: Expr -> EvalM Constant
evalExpr (Constant constant) = return constant
evalExpr (VAR (VarName varName)) = do
    (currentVarMap, _) <- get
    case M.lookup varName currentVarMap of
        Just element -> return element
        Nothing -> lift $ throwE $ VariableNotFound varName
evalExpr (BinOP op expr1 expr2) = do
    leftEl <- evalExpr expr1
    rightEl <- evalExpr expr2
    traceM ("Current bin operation: " ++ show op ++ " " ++ show leftEl ++ "  " ++ show rightEl) 
    case op of
        Plus -> return $ plus leftEl rightEl
        Equal -> return $ equal leftEl rightEl
evalExpr (UnOp op expr) = do 
    res <- evalExpr expr
    case op of
        Hd -> return $ headOp res
        Tl -> return $ tailOp res

equal :: Constant -> Constant -> Constant
equal x y = if x == y then IntConst 1 else IntConst 0

plus :: Constant -> Constant -> Constant
plus (IntConst intLeft) (IntConst intRight) = IntConst $ intLeft + intRight
plus (List listLeft) (List listRight) = List $ listLeft ++ listRight
plus (StrConst strLeft) (StrConst strRight) = StrConst $ strLeft ++ strRight

headOp :: Constant -> Constant
headOp (List (a : aTail)) = a
headOp _ = undefined

tailOp :: Constant -> Constant
tailOp (List (a : aTail)) = List aTail
tailOp (StrConst (a : aTail)) = StrConst aTail

eval ::  Program -> VarMap -> IO (Either Error Constant)
eval program varInit = runExceptT (evalStateT (evalVarMap program varInit) (M.empty, M.empty))