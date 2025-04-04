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
v name = VAR $ VarName name

-- Assigment
infixl 5 #=

-- equal
(#=) :: forall a. (Typeable a) =>  String -> a -> Assigment
varName #= expr = Assigment (VarName varName) (toExprEqual expr)

toExprEqual :: forall a. (Typeable a) => a -> Expr
toExprEqual expr
  | Just x <- cast expr = Constant (IntConst x)
  | Just x <- cast expr = VAR (VarName x)
  | Just x <- cast expr = x
  | otherwise           = error "Unsupported type"

infixl 5 ?=

-- isequal
(?=) :: forall a. (Typeable a) => Expr -> a -> Expr
varExpr ?= expr = BinOP EQUAL varExpr (toExprIsEqual expr)

toExprIsEqual :: forall a. (Typeable a) => a -> Expr
toExprIsEqual expr
  | Just x <- cast expr = Constant (IntConst x)
  | Just x <- cast expr = Constant (StrConst x)
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
bl labelName assigments = BasicBlock (Label labelName) assigments EMPTYJUMP

block :: [Assigment] -> BasicBlock
block assigments = BasicBlock EmptyLabel assigments EMPTYJUMP


-- Return
returnCnst :: String -> Jump
returnCnst str = RETURN (VAR (VarName str))

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
drpWhile = BinOP DROPWHILE 

drp :: Expr -> Expr -> Expr
drp = BinOP DROP 

pl :: Expr -> Expr -> Expr
pl = BinOP PLUS

-- Jump
if' :: Expr -> String -> String -> Jump
if' expr str1 str2 = IF expr (Label str1) (Label str2)

goto :: String -> Jump
goto label = GOTO $ Label label