{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Dsl where
import Ast

program :: [String] -> [BasicBlock] -> Program
program vars = Program (map VarName vars)

-- Assigment
assigment :: String -> Expr -> Assigment
assigment varName = Assigment (VarName varName)

constIntAssigment :: String -> Int -> Assigment
constIntAssigment varName cnst = Assigment (VarName varName) (Constant (IntConst cnst))

varAssigment :: String -> String -> Assigment
varAssigment varName varAssign = Assigment (VarName varName) (VAR (VarName varAssign))

-- Block

blj :: String -> Jump -> BasicBlock
blj labelName = BasicBlock (Label labelName) []

bj :: [Assigment] -> Jump -> BasicBlock
bj = BasicBlock EmptyLabel 

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

-- BinOp
isEqual :: Expr -> Expr -> Expr
isEqual = BinOP Equal

-- Jump
if' :: Expr -> String -> String -> Jump
if' expr str1 str2 = IF expr (Label str1) (Label str2)