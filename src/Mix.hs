module Mix where

import Ast
import Dsl
import GHC.Real (reduce)
import Interpret

mix :: Program
mix = program ["program", "division", "vs_0"]
    [
        blja "init" [
            "pending" #= cons' (EConstant emptyList) (pair (EConstant $ s "initial") (v "vs_0")),
            "marked" #= ListC [ ],
            "residual" #= emptyList
        ] (goto "while-0") ,
        blj "while-0"
            (if' ("pending" ?= emptyList) "end-0" "begin-0" ),
        blja "begin-0" [
            "pair_pend" #= hd (v "pending"),
            "pp" #= hd (v "pair_pend"),
            "vs" #= hd (tl (v "pair_pend")),
            "pending" #= tl (v "pending"),
            "marked" #= cons' (v "marked") (v "pair_pend"),
            "bb" #= lookup' (v "program") (v "pp"),
            "label_name_bb"  #= hd (v "bb"),
            "bb" #= tl (v "bb"),
            "code" #=  cons' (EConstant emptyList) (genLabel $ pair (v "label_name_bb") (v "vs")) -- code : initial_code(pp, vs)
            ] (goto "while-1"),
        blj "while-1" (if' ("bb" ?= emptyList) "end-1" "begin-1"),
        blja "begin-1" [
            "command" #= hd (v "bb"),
            "bb" #= tl (v "bb"),
            "type_command" #= hd (v "command")] (goto "case_assigment"),

        blj "case_assigment" (if' ("type_command" ?= s "assigment") "assigment-0" "case_goto"),
        blj "case_goto"      (if' ("type_command" ?= s "goto") "goto-0" "case_if"),
        blj "case_if"        (if' ("type_command" ?= s "if") "if-0" "case_return"),
        blj "case_return"    (if' ("type_command" ?= s "return") "return-0" "case_error"),
        blj "case_error"     (Return (pl (EConstant $ s "Coomand unexpected: ") (v "type_command"))),

        blja "assigment-0" [
            "X" #= hd (tl (v "command")),
            "exp" #= hd (tl (tl (v "command"))),
            "is_static" #= v "X" `isStatic'` v "division"
            ] (if' (v "is_static" ?= True) "assigment-true" "assigment-false"),
        blja "assigment-true" [
            "evalX" #= eval' (v "exp") (v "vs"),
            "vs" #= insert' (v "X") (v "evalX") (v "vs")
            ] (goto "while-1"),
        blja "assigment-false" [
            "reduceX" #= reduce' (v "exp") (v "vs"),
            "code" #= cons' (v "code") (list' [EConstant $ s "assigment", v "X", v "reduceX"]) -- to code. 
            ] (goto "while-1"),

        blja "if-0" [
            "exp" #= hd (tl (v "command")),
            "true_label" #= hd (tl (tl (v "command"))),
            "false_label" #= hd (tl (tl (tl (v "command")))),
            "is_static" #= v "exp" `isStatic'` v "division"
            ] (if' (v "is_static" ?= True) "if_static" "if_dynamic"),

        blja "if_static" [
            "if_res" #= eval' (v "exp") (v "vs")
            ] (if' (v "if_res" ?= True) "if_true" "if_false"),
        blja "if_true" [
            "bb" #= lookup' (v "program") (v "true_label"),
            "label_name_bb"  #= hd (v "bb"),
            "bb" #= tl (v "bb")
            ] (goto "while-1"),
        blja "if_false" [
            "bb" #= lookup' (v "program") (v "false_label"),
            "label_name_bb"  #= hd (v "bb"),
            "bb" #= tl (v "bb")
            ] (goto "while-1"),

        blja "if_dynamic" [
            "reduceExpr" #= reduce' (v "exp") (v "vs"),
            "code" #= cons' (v "code") (list' [EConstant $ s "if", v "reduceExpr", genLabel $ pair (v "true_label") (v "vs"), genLabel $ pair (v "false_label") (v "vs")]),
            "true_pair" #= pair (v "true_label") (v "vs"),
            "false_pair" #= pair (v "false_label") (v "vs")
            ] (goto "add_true_label_if"),

        blja "add_true_label_if" [
            "is_true_elem" #= v "true_pair" `elem'` v "marked"
            ] (if' (v "is_true_elem" ?= True) "add_false_label_if" "add_true_label"),
        blja "add_true_label" [
            "pending" #= cons' (v "pending") (v "true_pair")
            ] (goto "add_false_label_if"),    

        blja "add_false_label_if" [
            "is_false_elem" #= v "false_pair" `elem'` v "marked"
            ] (if' (v "is_false_elem" ?= True) "while-1" "add_false_label"),
        blja "add_false_label" [
            "pending" #= cons' (v "pending") (v "false_pair")
            ] (goto "while-1"),  

        blja "goto-0" [
            "pp_" #= hd ( tl (v "command")),
            "bb" #= lookup' (v "program") (v "pp_"),
            "label_name_bb"  #= hd (v "bb"),
            "bb" #= tl (v "bb")
            ] (goto "while-1"),    

        blja "return-0" [
            "rexpr" #= hd (tl (v "command")),
            "reduceRexpr" #= reduce' (v "rexpr") (v "vs"),
            "code" #= cons' (v "code") (list' [EConstant $ s "return", v "reduceRexpr"])
            ] (goto "while-1"),        

        blj "end-1" (goto "residual"),
        blja "residual" [
            "residual" #= cons' (v "residual") (v "code")
            ] (goto "while-0"),
        blja "end-0" [
            "residual_program" #= toprogram (v "residual") (v "division") (v "program")
            ] (Return (v "residual_program"))
    ]

