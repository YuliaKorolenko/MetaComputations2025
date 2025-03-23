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

data Op = Plus deriving Show
    -- = HD 
    -- | TL 
    -- | CONS
    -- deriving Show

data Expr 
    = CONST Int |
    VAR VarName |
    BinOP Op Expr Expr
    deriving (Show)

data Label = Label String | EmptyLabel deriving (Show)