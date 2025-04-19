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
    deriving (Show, Eq)

newtype VarName = VarName String deriving (Show, Eq, Ord)

data Assigment = Assigment { var :: VarName, expr :: Expr } deriving (Show, Eq)

data BinOp = Plus |
             Equal | 
             DropWhile | 
             Drop | 
             Union |
             Lookup |
             Elem |
             IsStatic |
             Eval |
             Reduce |
             Except |
             Cons
                deriving (Show, Eq)

data UnOp = Hd |
            Tl |
            ToPrgrm |
            GenLabel  
            deriving (Show, Eq)

data TernOp = Insert 
              deriving (Show, Eq)

data Expr = EConstant Constant |
            EVar VarName |
            EBinOP BinOp Expr Expr |
            EUnOp UnOp Expr |
            ETernOp TernOp Expr Expr Expr 
                deriving (Show, Typeable, Eq)

data Constant = IntC Int | 
                ListC [Constant] | 
                StrC String |  
                ExprC Expr |
                ProgramC Program |
                BoolC Bool
                    deriving (Show, Eq)

data Label = Label String | 
             EmptyLabel 
                deriving (Show, Eq)