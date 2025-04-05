module Division where

import Debug.Trace (traceM, trace)

import Ast
import Dsl
import Data.List (nub, sort, (\\), intersect, length)

-- generate all vars 
allProgramVars :: Program -> [VarName]
allProgramVars (Program varnames blocks) = nub $ varnames ++ concatMap genBlockVars blocks
    where
        genBlockVars :: BasicBlock -> [VarName]
        genBlockVars (BasicBlock _ assignments _) =
            map (\(Assigment varname _) -> varname) assignments

generateStaticVars :: Program -> [VarName] -> [VarName]
generateStaticVars prog@(Program varnames blocks) staticVars = allProgramVars prog \\ iterateDynamicVars blocks (varnames \\ staticVars)

iterateDynamicVars :: [BasicBlock] -> [VarName] -> [VarName]
iterateDynamicVars blocks dinamicVars =
    let dinamicVars' = executeProgram blocks dinamicVars
    in
        if sort dinamicVars /= sort dinamicVars'
        then iterateDynamicVars blocks dinamicVars'
        else dinamicVars'

executeProgram :: [BasicBlock] -> [VarName] -> [VarName]
executeProgram ((BasicBlock _ assigments _) : tailBlocks) dynamicVars = executeProgram tailBlocks (exAssigment assigments dynamicVars)
executeProgram [] dynamicVars = dynamicVars

exAssigment :: [Assigment] -> [VarName] -> [VarName]
exAssigment (Assigment var expr : tailAssigm) dynamicVars = do
    let intersectLen = length $ expVarNames expr `intersect` dynamicVars
    let isDynamic = var `elem` dynamicVars
    if intersectLen /= 0 && not isDynamic
    then exAssigment tailAssigm (var : dynamicVars)
    else exAssigment tailAssigm dynamicVars
exAssigment [] dynamicVars = dynamicVars

expVarNames :: Expr -> [VarName]
expVarNames (EConstant _ ) = []
expVarNames (EVar v) = [v]
expVarNames (EBinOP _ expr1 expr2) = expVarNames expr2 ++ expVarNames expr1
expVarNames (EUnOp _ expr) = expVarNames expr
