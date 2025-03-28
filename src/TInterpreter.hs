module TInterpreter where

import Dsl
import Ast

-- (VAR $ VarName "QTail")
-- [IF (isEqual (Constant $ List []) (Constant $ List [])) "stop" "cons"]

turingInterpreter :: Program
turingInterpreter = program ["Q", "Right"] 
                    [
                        bl "init" [varAssigment "QTail" "Q", assigment "Left" (Constant $ List [])],  -- init: QTail := Q; Left := '();
                        blj "loop" (if' (isEqual (VAR $ VarName "QTail") (Constant $ List [])) "stop" "cons"), -- "loop: if QTail = '() goto stop else cont;"
                        blj "stop" (returnCnst "Right")-- stop : return right;
                    ] 
