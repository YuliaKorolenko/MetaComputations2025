module InterpretOp where

import Ast
import Data.List(nub, find)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Debug.Trace (trace)
import Data.Map (Map)


equal :: Constant -> Constant -> Constant
equal x y =
    if x == y
    then
        -- trace ("Equal: " ++ show x ++ "=" ++ show y )
        IntC 1
    else
        -- trace ("Not equal: " ++ show x ++ "!=" ++ show y )
        IntC 0

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

lookupOp :: Constant -> Constant -> Constant
lookupOp (ProgramC (Program _ basicBlocks)) (StrC label)=
    let labeledBlock = find (\(BasicBlock l _ _) -> l == Label label) basicBlocks
    in
    case labeledBlock of
        Just block ->
            -- trace ("In Lookup opertion: basick blocks:" ++ show basicBlocks ++ " block: " ++ show block)
            blockToCommandsList block
        Nothing ->
            -- trace ("Lookup op: " ++ show basicBlocks)
            error $ "Block with label: " ++ label ++ " not found."
lookupOp (ListC constantList) varnameToFind =
    let findVar = find (\(ListC [l, rest]) -> l == varnameToFind) constantList
    in case findVar of
        Just (ListC [curVar, expr]) -> expr
        Nothing -> error $ "Static varname " ++ show varnameToFind  ++ " not found."

-- First element in list will be indentificator: assigment, goto, if, return
blockToCommandsList :: BasicBlock -> Constant
blockToCommandsList (BasicBlock label assigments jump) =
    let assigmentList = map (\(Assigment varName expr) -> ListC [StrC "assigment", ExprC $ EVar varName , ExprC expr]) assigments
        jumpElement = jumpToCommand jump
    in if null jumpElement
       then ListC assigmentList
       else ListC $ assigmentList ++ [ListC jumpElement]

jumpToCommand :: Jump -> [Constant]
jumpToCommand (Goto (Label labelName)) = [StrC "goto", StrC labelName]
jumpToCommand (If expr1 (Label ifTrueLabel) (Label ifFalseLabel)) = [StrC "if", ExprC expr1, StrC ifTrueLabel, StrC ifFalseLabel]
jumpToCommand (Return expr) = [StrC "return", ExprC expr]
jumpToCommand EmptyJump = []

elemOp :: Constant -> Constant -> Constant
elemOp findEl l@(ListC (ListC [cuvar, expr]  : tail)) = if findEl == cuvar
                                       then BoolC True
                                       else elemOp findEl (ListC tail)
elemOp findEl l@(ListC (curEl : tail)) = if findEl == curEl
                                        then BoolC True
                                        else elemOp findEl (ListC tail)
elemOp findEl (ListC []) = BoolC False

varListToMap :: [Constant] -> M.Map String Expr
varListToMap constants =
    -- trace ("current map2: " ++ show (M.fromList (map extractVarNameAndValue constants)))
    M.fromList $ map extractVarNameAndValue constants
    where
        extractVarNameAndValue ((ListC ((ExprC (EVar (VarName varname))) : val : _))) = (varname, EConstant val)
        extractVarNameAndValue _ = error "Invalid structure in varListToMap"

insertOp :: Constant -> Constant -> Constant -> Constant
insertOp varToFind res (ListC varnames@(headVar@(ListC [curVar, _]) : tailVar)) =
    let isInList = elemOp varToFind (ListC varnames)
    in if isInList == BoolC True
       then ListC (map (insertInExpr varToFind res) varnames)
       else ListC $ varnames ++ [ListC [varToFind, res]]

insertInExpr :: Constant -> Constant -> Constant -> Constant
insertInExpr varname res (ListC [var, value])
    | var == varname = ListC [var, res]
    | otherwise      = ListC [var, value]