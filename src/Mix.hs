module Mix where

import Ast
import Dsl

mix :: Program
mix = program ["program", "division", "vs_0"]
    [
        bl "init" [
            "pending" #= ListC [pair (s "initial") "vs_0"],
            "marked" #= ListC []
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
            "code" #=  ListC [ListC [ExprC $ v "pp"]] -- code : initial_code(pp, vs)
            ] (goto "while-1"),
        blja "while-1" [
            "command" #= hd (v "bb"),
            "bb" #= tl (v "bb"),
            "type_command" #= hd (v "command")
            ] (goto "case-case_assigment"),
        blj "case_assigment" (if' (v "type_command" ?= "assigment-0") "assigment" "case_goto"),
        blj "case_goto" (if' (v "type_command" ?= "goto") "goto" "case_if"),
        blj "case_if" (if' (v "type_command" ?= "if") "if" "case_return"),
        blj "case_return" (if' (v "type_command" ?= "return") "return" "case_error"),
        blj "case_error" (Return (pl (EConstant $ s "Coomand unexpected: ") (v "type_command"))),

        blja "assigment-0" [
            "X" #= hd (tl (v "command")),
            "is_static" #= v "X" `elem'` v "division"] EmptyJump
    ]

