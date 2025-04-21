{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}


module Dsl where
import Ast
import Data.Typeable (Typeable, cast)
import Control.Monad.Except (Except)

program :: [String] -> [BasicBlock] -> Program
program vars = Program (map VarName vars)

v :: String -> Expr
v name = EVar $ VarName name

-- Assigment
infixl 5 #=

-- equal
(#=) :: forall a. (Typeable a) =>  String -> a -> Assigment
varName #= expr = Assigment (VarName varName) (toExprEqual expr)

toExprEqual :: forall a. (Typeable a) => a -> Expr
toExprEqual expr
  | Just x <- cast expr = EVar (VarName x)
  | Just x <- cast expr = EConstant (IntC x)
  | Just x <- cast expr = EConstant x
  | Just x <- cast expr = x
  | Just x <- cast expr = EConstant (BoolC x)
  | otherwise           = error "Unsupported type"

infixl 5 ?=

-- isequal
(?=) :: (Typeable a, Typeable b) => a -> b -> Expr
varExpr ?= expr = EBinOP Equal (toExprEqual varExpr) (toExprEqual expr)

(!=) :: (Typeable a, Typeable b) => a -> b -> Expr
varExpr != expr = EBinOP NotEqual (toExprEqual varExpr) (toExprEqual expr)

-- Return
returnCnst :: String -> Jump
returnCnst str = Return (EVar $ VarName str)

-- Block

blj :: String -> Jump -> BasicBlock
blj labelName = BasicBlock (Label labelName) []

blja :: String -> [Assigment] -> Jump -> BasicBlock
blja labelName = BasicBlock (Label labelName)

bj :: Jump -> BasicBlock
bj = BasicBlock EmptyLabel []

bja :: [Assigment] -> Jump -> BasicBlock
bja = BasicBlock EmptyLabel

-- Lists
emptyList :: Constant
emptyList = ListC []

lInt :: [Int] -> Constant
lInt elems = ListC (map IntC elems)

lStr :: [String] -> Constant
lStr elems = ListC (map StrC elems)

lConst :: [Expr] -> Expr
lConst exprs = EConstant $ ListC $ map exprToConstant exprs
  where exprToConstant :: Expr -> Constant
        exprToConstant (EConstant c) = c
        exprToConstant expr = ExprC expr

-- Operations

hd :: Expr -> Expr
hd = EUnOp Hd

tl :: Expr -> Expr
tl = EUnOp Tl

toprogram :: Expr -> Expr -> Expr -> Expr
toprogram = ETernOp ToPrgrm

cons' :: Expr -> Expr -> Expr
cons' = EBinOP Cons

drpWhile :: Expr -> Expr -> Expr
drpWhile = EBinOP DropWhile

drp :: Expr -> Expr -> Expr
drp = EBinOP Drop

lookup' :: Expr -> Expr -> Expr
lookup' = EBinOP Lookup

eval' :: Expr -> Expr -> Expr
eval' = EBinOP Eval

insert' :: Expr -> Expr -> Expr -> Expr
insert' = ETernOp Insert

reduce' :: Expr -> Expr -> Expr
reduce' = EBinOP Reduce

pl :: Expr -> Expr -> Expr
pl = EBinOP Plus

u :: Expr -> Expr -> Expr
u = EBinOP Union

isStatic' :: Expr -> Expr -> Expr
isStatic' = EBinOP IsStatic

elem' :: Expr -> Expr -> Expr
elem' = EBinOP Elem

-- Jump
if' :: Expr -> String -> String -> Jump
if' expr str1 str2 = If expr (Label str1) (Label str2)

goto :: String -> Jump
goto label = Goto $ Label label

-- Constant

s :: String -> Constant
s = StrC

pair :: Expr -> Expr -> Expr
pair expr1 expr2 = EConstant (ListC []) `cons'` expr1 `cons'` expr2

list' :: [Expr] -> Expr
list' = foldl cons' (EConstant (ListC []))

genLabel :: Expr -> Expr
genLabel = EUnOp GenLabel