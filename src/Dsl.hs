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
v name = Var $ VarName name

-- Assigment
infixl 5 #=

-- equal
(#=) :: forall a. (Typeable a) =>  String -> a -> Assigment
varName #= expr = Assigment (VarName varName) (toExprEqual expr)

toExprEqual :: forall a. (Typeable a) => a -> Expr
toExprEqual expr
  | Just x <- cast expr = Constant (IntConst x)
  | Just x <- cast expr = Constant x
  | Just x <- cast expr = Var (VarName x)
  | Just x <- cast expr = x
  | otherwise           = error "Unsupported type"

infixl 5 ?=

-- isequal
(?=) :: (Typeable a, Typeable b) => a -> b -> Expr
varExpr ?= expr = BinOP Equal (toExprIsEqual varExpr) (toExprIsEqual expr)

toExprIsEqual :: forall a. (Typeable a) => a -> Expr
toExprIsEqual expr
  | Just x <- cast expr = Constant (IntConst x)
  | Just x <- cast expr = Var (VarName x)
  | Just x <- cast expr = Constant x
  | Just x <- cast expr = x
  | otherwise           = error "Unsupported type"

-- Block

blj :: String -> Jump -> BasicBlock
blj labelName = BasicBlock (Label labelName) []

blja :: String -> [Assigment] -> Jump -> BasicBlock
blja labelName = BasicBlock (Label labelName)

bj :: Jump -> BasicBlock
bj = BasicBlock EmptyLabel []

bja :: [Assigment] -> Jump -> BasicBlock
bja = BasicBlock EmptyLabel

bl :: String -> [Assigment] -> BasicBlock
bl labelName assigments = BasicBlock (Label labelName) assigments EmptyJump

block :: [Assigment] -> BasicBlock
block assigments = BasicBlock EmptyLabel assigments EmptyJump


-- Return
returnCnst :: String -> Jump
returnCnst str = Return (Var (VarName str))

-- Lists
lInt :: [Int] -> Constant
lInt elems = List (map IntConst elems)

lStr :: [String] -> Constant
lStr elems = List (map StrConst elems)

hd :: Expr -> Expr
hd = UnOp Hd

tl :: Expr -> Expr
tl = UnOp Tl

cons :: Constant -> Expr -> Expr
cons cnst = pl (Constant $ List [cnst])

drpWhile :: Expr -> Expr -> Expr
drpWhile = BinOP DropWhile

drp :: Expr -> Expr -> Expr
drp = BinOP Drop

lookup' :: Expr -> Expr -> Expr
lookup' = BinOP Lookup

pl :: Expr -> Expr -> Expr
pl = BinOP Plus

u :: Expr -> Expr -> Expr
u = BinOP Union

-- Jump
if' :: Expr -> String -> String -> Jump
if' expr str1 str2 = If expr (Label str1) (Label str2)

goto :: String -> Jump
goto label = Goto $ Label label

-- Constant

s :: String -> Constant
s = StrConst

pair :: Constant -> String -> Constant
pair const1 varName = List [const1, Expr $ v varName]