module Mix where

import Ast
import Dsl

mix :: Program
mix = program ["program", "division", "vs_0"]
    [
        bl "init" [
            "pending" #= List [pair (s "initial") "vs_0"],
            "marked" #= List []
        ],
        blj "while-0" 
            (if' ("pending" ?=  List []) "begin-0" "end-0"),
        bl "begin-0" [
            "pair_pend" #= hd (v "pending"),
            "pp" #= hd (v "pair_pend"),
            "vs" #= tl (v "pair_pend"),
            "pending" #= tl (v "pending"),
            "marked" #= v "pair_pend" `u` v "marked",
            "bb" #= lookup' (v "pp") (v "program")
        ]
    ]

