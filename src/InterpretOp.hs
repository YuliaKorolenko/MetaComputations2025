module InterpretOp where

import Ast
import Data.List(nub, find)
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Debug.Trace (trace)
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Ord (comparing)
import qualified Data.List as L

equal :: Constant -> Constant -> Constant
equal x y =
    if x == y
    then
        -- trace ("Equal: " ++ show x ++ "=" ++ show y )
        BoolC True
    else
        -- trace ("Not equal: " ++ show x ++ "!=" ++ show y )
        BoolC False

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
headOp (ListC []) =
    -- trace ("IN EMPTY HEAD OP")
    undefined

tailOp :: Constant -> Constant
tailOp (ListC (a : aTail)) = ListC aTail
tailOp (StrC (a : aTail)) = StrC aTail
tailOp (ListC []) = undefined

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
blockToCommandsList (BasicBlock (Label labelName) assigments jump) =
    let assigmentList = map (\(Assigment varName expr) -> ListC [StrC "assigment", ExprC $ EVar varName , ExprC expr]) assigments
        jumpElement = jumpToCommand jump
    in ListC $ [StrC labelName] ++ assigmentList ++ [ListC jumpElement]

commandsListToBlock :: Constant -> BasicBlock
commandsListToBlock (ListC ((StrC name):rest)) = do
    let (assigmentCmds, jumpCmd) = case reverse rest of
            [] -> ([], [])
            (lastCmd:revAssigs) -> (reverse revAssigs, [lastCmd])
    let assigments = map commandToAssigment assigmentCmds

    let jump = commandToJump $ head jumpCmd
    BasicBlock (Label name) assigments jump
commandsListToBlock el = trace ("COMMANDS LIST TO BLOCK: " ++ show el)
                         undefined

commandToAssigment :: Constant -> Assigment
commandToAssigment (ListC [StrC "assigment", ExprC (EVar varName), ExprC expr]) = Assigment varName expr

commandToJump :: Constant -> Jump
commandToJump (ListC [StrC "goto", StrC labelName]) = Goto (Label labelName)
commandToJump (ListC [StrC "if", ExprC expr, StrC ifTrueLabel, StrC ifFalseLabel]) = If expr (Label ifTrueLabel) (Label ifFalseLabel)
commandToJump (ListC [StrC "return", ExprC expr]) = Return expr
commandToJump (ListC [StrC "return", cnst]) = Return $ EConstant cnst

jumpToCommand :: Jump -> [Constant]
jumpToCommand (Goto (Label labelName)) = [StrC "goto", StrC labelName]
jumpToCommand (If expr1 (Label ifTrueLabel) (Label ifFalseLabel)) = [StrC "if", ExprC expr1, StrC ifTrueLabel, StrC ifFalseLabel]
jumpToCommand (Return expr) = [StrC "return", ExprC expr]

findByLabelOp :: Constant -> Constant -> Constant
findByLabelOp findEl l@(ListC (ListC [cuvar, expr]  : tail)) = if findEl == cuvar
                                       then BoolC True
                                       else findByLabelOp findEl (ListC tail)
findByLabelOp findEl l@(ListC (curEl : tail)) = if findEl == curEl
                                        then BoolC True
                                        else findByLabelOp findEl (ListC tail)
findByLabelOp findEl (ListC []) = BoolC False

elemOp :: Constant -> Constant -> Constant
elemOp findEl (ListC (curEl : tail)) =
    if findEl == curEl
    then BoolC True
    else elemOp findEl (ListC tail)
elemOp _ (ListC []) = BoolC False

checkAllVars :: Constant -> Constant -> Constant
checkAllVars (ExprC expr) constant =
    let vars = collectVars expr
    in BoolC $ all (\var -> findByLabelOp (ExprC (EVar var)) constant == BoolC True) vars
checkAllVars el constant = trace ("COMMANDS checkAllVars: " ++ show el ++ "  " ++ show constant)
                  undefined

collectVars :: Expr -> Set.Set VarName
collectVars expr = case expr of
    EConstant _      -> Set.empty
    EVar var         -> Set.singleton var
    EBinOP _ e1 e2   -> Set.union (collectVars e1) (collectVars e2)
    EUnOp _ e        -> collectVars e
    ETernOp _ e1 e2 e3 -> Set.unions [collectVars e1, collectVars e2, collectVars e3]

varListToMap :: [Constant] -> M.Map String Expr
varListToMap constants =
    -- trace ("current map2: " ++ show (M.fromList (map extractVarNameAndValue constants)))
    M.fromList $ map extractVarNameAndValue constants
    where
        extractVarNameAndValue ((ListC ((ExprC (EVar (VarName varname))) : val : _))) = (varname, EConstant val)
        extractVarNameAndValue _ = error "Invalid structure in varListToMap"

insertOp :: Constant -> Constant -> Constant -> Constant
insertOp varToFind res (ListC varnames@(headVar@(ListC [curVar, _]) : tailVar)) =
    let isInList = findByLabelOp varToFind (ListC varnames)
    in if isInList == BoolC True
       then ListC (map (insertInExpr varToFind res) varnames)
       else ListC $ varnames ++ [ListC [varToFind, res]]

toProgramOp ::  Constant -> Constant
toProgramOp (ListC blockConstants)  = ProgramC $ Program [] (map commandsListToBlock blockConstants)

insertInExpr :: Constant -> Constant -> Constant -> Constant
insertInExpr varname res (ListC [var, value])
    | var == varname = ListC [var, res]
    | otherwise      = ListC [var, value]

consOp :: Constant -> Constant -> Constant
consOp (ListC []) smth = ListC [smth]
consOp (ListC xs) newElem =
--   trace ("Apply cons operation: " ++ show xs ++ "list: " ++ show newElem)
  ListC (xs ++ [newElem])
consOp _ _ = error "Left-hand side must be a ListC"

genLabelOp :: Constant -> Constant
genLabelOp (ListC [StrC label, el2]) = StrC ("(" ++ label ++ ", " ++ extractAndSortValues el2 ++ ")")
genLabelOp el = trace ("GENERATE LABEL OP: " ++ show el)
             undefined

extractAndSortValues :: Constant -> String
extractAndSortValues (ListC items) =
    let pairs = concatMap extractPairs items
        sorted = L.sortBy (comparing fst) pairs
        elements = map (\(name, val) -> name ++ "=" ++ showConstant val) sorted
    in L.intercalate "; " elements
extractAndSortValues _ = error "Invalid vs0 structure"

extractPairs :: Constant -> [(String, Constant)]
extractPairs (ListC [ExprC (EVar (VarName name)), val]) = [("'" ++ name ++ "'" , val)]

showConstant :: Constant -> String
showConstant (ListC items) = "[" ++ L.intercalate "," (map showConstant items) ++ "]"
showConstant (IntC i) = show i
showConstant (StrC s) = s
showConstant (ExprC e) = show e

