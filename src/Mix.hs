module Mix where

import Ast
import Dsl
import GHC.Real (reduce)
import Interpret

mix :: Program
mix = program ["program", "division", "vs_0"]
    [
        blja "init" [
            "pending" #= ListC [pair (s "initial") "vs_0"],
            "marked" #= ListC [],
            "residual" #= ListC []
        ] (goto "while-0") ,
        blj "while-0"
            (if' ("pending" ?=  ListC []) "end-0" "begin-0" ),
        blja "begin-0" [
            "pair_pend" #= hd (v "pending"),
            "pp" #= hd (v "pair_pend"),
            "vs" #= hd (tl (v "pair_pend")),
            "pending" #= tl (v "pending"),
            "marked" #= v "pair_pend" `u` v "marked",
            "bb" #= lookup' (v "program") (v "pp"),
            "label_name_bb"  #= hd (v "bb"),
            "bb" #= tl (v "bb"),
            "code" #=  cons' (EConstant (ListC [])) [v "label_name_bb"] -- code : initial_code(pp, vs)
            ] (goto "while-1"),
        blj "while-1" (if' ("bb" ?= ListC []) "end-1" "begin-1"),
        blja "begin-1" [
            "command" #= hd (v "bb"),
            "bb" #= tl (v "bb"),
            "type_command" #= hd (v "command")] (goto "case_assigment"),

        blj "case_assigment" (if' ("type_command" ?= s "assigment") "assigment-0" "case_goto"),
        blj "case_goto"      (if' ("type_command" ?= s "goto") "goto-0" "case_if"),
        blj "case_if"        (if' ("type_command" ?= s "if") "if" "case_return"),
        blj "case_return"    (if' ("type_command" ?= s "return") "return-0" "case_error"),
        blj "case_error"     (Return (pl (EConstant $ s "Coomand unexpected: ") (v "type_command"))),

        blja "assigment-0" [
            "X" #= hd (tl (v "command")),
            "exp" #= hd (tl (tl (v "command"))),
            "is_static" #= v "X" `elem'` v "division"
            ] (if' (v "is_static" ?= True) "assigment-true" "assigment-false"),
        blja "assigment-true" [
            "evalX" #= eval' (v "exp") (v "vs"),
            "vs" #= insert' (v "X") (v "evalX") (v "vs")
            ] (goto "while-1"),
        blja "assigment-false" [
            "reduceX" #= reduce' (v "exp") (v "vs"),
            "code" #= cons' (v "code") [EConstant $ s "assigment", v "X", v "reduceX"] -- to code. 
            ] (goto "while-1"),

        blja "goto-0" [
            "pp_" #= hd ( tl (v "command")),
            "bb" #= lookup' (v "program") (v "pp_"),
            "label_name_bb"  #= hd (v "bb"),
            "bb" #= tl (v "bb"),
            "code" #=  cons' (EConstant (ListC [])) [v "label_name_bb"]
            ] (goto "while-1"),    

        blja "return-0" [
            "rexpr" #= hd (tl (v "command")),
            "reduceRexpr" #= reduce' (v "rexpr") (v "vs"),
            "code" #= cons' (v "code") [EConstant $ s "return", v "reduceRexpr"]
            ] (goto "while-1"),        

        blj "end-1" (goto "residual"),
        blja "residual" [
            "residual" #= cons' (v "residual") [v "code"]
            ] (goto "while-0"),
        blja "end-0" [
            "residual_program" #= toprogram (v "residual")
            ] (Return (v "residual_program"))
    ]

