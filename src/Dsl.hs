{-# LANGUAGE OverloadedStrings #-}
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
blockLabJump :: String -> [Assigment] -> Jump -> BasicBlock
blockLabJump labelName = BasicBlock (Label labelName)

blockJump :: [Assigment] -> Jump -> BasicBlock
blockJump = BasicBlock EmptyLabel 

block :: [Assigment] -> BasicBlock
block assigments = BasicBlock EmptyLabel assigments EMPTYJUMP


-- Return
returnCnst :: String -> Jump
returnCnst str = RETURN (VAR (VarName str))