{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Ast where
import Data.Typeable (Typeable, cast)


data Program = Program [VarName] [BasicBlock] 
    deriving (Show, Eq)

data BasicBlock = BasicBlock Label [Assigment] Jump 
    deriving (Show, Eq)

data Jump 
    = Goto Label 
    | If Expr Label Label 
    | Return Expr 
    | EmptyJump
    deriving (Show, Eq)

newtype VarName = VarName String deriving (Show, Eq, Ord)

data Assigment = Assigment { var :: VarName, expr :: Expr } deriving (Show, Eq)

data BinOp = Plus |
             Equal | 
             DropWhile | 
             Drop | 
             Union |
             Lookup
                deriving (Show, Eq)

data UnOp = Hd |
            Tl deriving (Show, Eq)

data Constant = IntConst Int | 
                List [Constant] | 
                StrConst String |  
                Expr Expr |
                ProgramC Program
                    deriving (Show, Eq)

data Expr = Constant Constant |
            Var VarName |
            BinOP BinOp Expr Expr |
            UnOp UnOp Expr 
                deriving (Show, Typeable, Eq)

data Label = Label String | 
             EmptyLabel 
                deriving (Show, Eq)