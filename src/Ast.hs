{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Ast where


data Program = Program [VarName] [BasicBlock]

data BasicBlock = BasicBlock Label [Assigment] Jump 
    deriving (Show)

data Jump 
    = GOTO Label 
    | IF Expr Label Label 
    | RETURN Expr 
    | EMPTYJUMP
    deriving (Show)

newtype VarName = VarName String deriving (Show)

data Assigment = Assigment { var :: VarName, expr :: Expr } deriving (Show)

data BinOp = Plus | Equal deriving Show 

data UnOp = Hd |
            Tl deriving Show

data Constant = IntConst Int | List [Constant] | StrConst String deriving (Show, Eq)

data Expr 
    = Constant Constant |
    VAR VarName |
    BinOP BinOp Expr Expr |
    UnOp UnOp Expr
    deriving (Show)

data Label = Label String | EmptyLabel deriving (Show)