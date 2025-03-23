{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Interpret where

import Control.Monad.State (StateT, put, MonadState(get), MonadTrans(lift), evalStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M

import Ast

data Error = UndefinedVar String | VariableNotFound String | UnexpectedElement String deriving (Show)

type VarMap = M.Map String Int
type LabelMap = M.Map String [BasicBlock]

type EvalM = StateT (VarMap, LabelMap) (ExceptT Error IO)                           

evalVarMap :: Program -> VarMap -> EvalM Int
evalVarMap (Program ((VarName varName) : varTail) basicBlocks) varMap =
    case M.lookup varName varMap of
        Just val -> do
            liftIO $ putStrLn ("Curren var : " ++ show varName ++ " " ++ show val)
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


handleLabel :: Label -> Jump -> EvalM Int
handleLabel EmptyLabel instr  = 
    lift $ throwE $ UnexpectedElement ("Expected a non-empty label, but got EmptyLabel in '" ++ show instr ++ "'instruction.")
handleLabel (Label labelName) _ = do
    (_, currentLabelMap) <- get
    case M.lookup labelName currentLabelMap of
        Just basicBlocks -> evalBasicBlocks basicBlocks
        Nothing -> lift $ throwE $ UnexpectedElement ("Expected a label named '" ++ labelName ++ "', but it was not found in the LabelMap.")

handleJumpBlock :: BasicBlock -> EvalM Int
handleJumpBlock (BasicBlock _ _ instr@(GOTO goLabel)) = handleLabel goLabel instr
handleJumpBlock (BasicBlock _ _ instr@(IF curExpr trueLabel falseLabel)) = do
    val <- evalExpr curExpr
    case val of
        1 -> handleLabel trueLabel instr
        _ -> handleLabel falseLabel instr
handleJumpBlock (BasicBlock _ _ (RETURN curExpr)) = evalExpr curExpr
handleJumpBlock _ = lift $ throwE $ UnexpectedElement "In jump blocks"

evalBasicBlocks :: [BasicBlock] -> EvalM Int
evalBasicBlocks (BasicBlock _ assigments EMPTYJUMP : tailBlocks) = do
    evalAssigments assigments
    evalBasicBlocks tailBlocks
evalBasicBlocks (block@(BasicBlock _ assigments _) : _) = do
    evalAssigments assigments
    handleJumpBlock block
evalBasicBlocks [] =  lift $ throwE $ UnexpectedElement "In eval basic blocks"

evalAssigments :: [Assigment] -> EvalM ()
evalAssigments (Assigment (VarName varName) expr1 : assigmentTail) = do
    result <- evalExpr expr1
    (currentVarMap, currentLabelMap) <- get
    let updatedVarMap = M.insert varName result currentVarMap
    put (updatedVarMap, currentLabelMap)
    evalAssigments assigmentTail
evalAssigments [] = return ()

evalExpr :: Expr -> EvalM Int
evalExpr (CONST constant) = return constant
evalExpr (VAR (VarName varName)) = do
    (currentVarMap, _) <- get
    case M.lookup varName currentVarMap of
        Just element -> return element
        Nothing -> lift $ throwE $ VariableNotFound varName
evalExpr (BinOP op expr1 expr2 ) = case op of
    Plus -> do 
        leftEl <- evalExpr expr1
        rightEl <- evalExpr expr2
        return (leftEl + rightEl)


eval ::  Program -> VarMap -> IO (Either Error Int)
eval program varInit = runExceptT (evalStateT (evalVarMap program varInit) (M.empty, M.empty))