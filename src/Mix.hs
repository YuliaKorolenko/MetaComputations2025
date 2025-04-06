module Mix where

import Ast
import Dsl
import GHC.Real (reduce)

mix :: Program
mix = program ["program", "division", "vs_0"]
    [
        bl "init" [
            "pending" #= ListC [pair (s "initial") "vs_0"],
            "marked" #= ListC [],
            "residual" #= ListC []
        ],
        blj "while-0"
            (if' ("pending" ?=  ListC []) "begin-0" "end-0"),
        blja "begin-0" [
            "pair_pend" #= hd (v "pending"),
            "pp" #= hd (v "pair_pend"),
            "vs" #= tl (v "pair_pend"),
            "pending" #= tl (v "pending"),
            "marked" #= v "pair_pend" `u` v "marked",
            "bb" #= lookup' (v "program") (v "pp"),
            "code" #=  ListC [ListC []] -- code : initial_code(pp, vs)
            ] (goto "while-1"),
        blja "while-1" [
            "command" #= hd (v "bb"),
            "bb" #= tl (v "bb"),
            "type_command" #= hd (v "command")
            ] (if' ("bb" ?= ListC []) "end-1" "case-case_assigment"),

        blj "case_assigment" (if' ("type_command" ?= "assigment-0") "assigment" "case_goto"),
        blj "case_goto"      (if' ("type_command" ?= "goto") "goto" "case_if"),
        blj "case_if"        (if' ("type_command" ?= "if") "if" "case_return"),
        blj "case_return"    (if' ("type_command" ?= "return") "return" "case_error"),
        blj "case_error"     (Return (pl (EConstant $ s "Coomand unexpected: ") (v "type_command"))),

        blja "assigment-0" [
            "X" #= hd (tl (v "command")),
            "exp" #= tl (tl (v "command")),
            "is_static" #= v "X" `elem'` v "division"
            ] (if' (v "is_static" ?= True) "assigment-true" "assigment-false"),
        blja "assigment-true" [
            "evalX" #= eval' (v "exp") (v "vs"),
            "vs" #= insert' (v "X") (v "evalX") (v "vs")
            ] (goto "while-1"),
        blja "assigment-false" [
            "reduceX" #= reduce' (v "exp") (v "vs"),
            "code" #= cons' (v "code") (lConst [EConstant $ s "assigment", v "X", v "reduceX"]) -- to code. 
            ] (goto "while-1"),

        blj "end-1" (goto "resudial"),
        blja "residual" [
             "residual" #= cons' (v "residual") (v "code")
            ] (goto "while-0"),
        blj "end-0" (Return (v "residual"))
    ]

