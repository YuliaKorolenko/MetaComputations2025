module TInterpreter where

import Dsl
import Ast

turingInterpreter :: Program
turingInterpreter = program ["Q", "Right"]
                    [
                        blja  "initial"  ["QTail" #= "Q", "Left" #= ListC []] (goto "loop"),   -- init: QTail := Q; Left := '();
                        blj "loop"  (if' (v "QTail" ?= ListC []) "stop" "cont"),                   -- "loop: if QTail = '() goto stop else cont;"
                        blj "stop"  (returnCnst "Right"),                                                -- stop : return right;
                        blja  "cont"  ["Instruction" #= hd (v "QTail"),               -- cont : Instruction := First_instruction(QTail);
                                     "QTail"       #= tl (v "QTail"),                      -- QTail := rest(Qtail);
                                     "Operator"    #= hd (v "Instruction")] (goto "cont0"),      -- Operator := hd(tl(Instruction)); Почему hd(tl)?
                        blj "cont0" (if' ("Operator" ?= s "right") "do-right" "cont1"),  -- if Operator = 'right goto do-right else cont1;
                        blj "cont1" (if' ("Operator" ?= s "left") "do-left" "cont2"),    -- cont1: if Operator = 'left goto do-left else cont2;     
                        blj "cont2" (if' ("Operator" ?= s "write") "do-write" "cont3"),  -- cont2: if Operator = 'write goto do-write else cont3;   
                        blj "cont3" (if' ("Operator" ?= s "goto") "do-goto" "cont4"),    -- cont3: if Operator = 'goto goto do-goto else cont4;   
                        blj "cont4" (if' ("Operator" ?= s "if") "do-if" "error"),         -- cont4: if Operator = 'if goto do-if else error;  

                        blja "do-right" ["Left" #= pl (hd (v "Right")) (v "Left"), -- do-right: Left := cons(firstsym(Right), Left);
                                         "Right" #= tl (v "Right")]  -- Right:= tl(Right) 
                                        (goto "loop"),       -- goto loop;

                        blja "do-left" ["Right" #= pl (hd (v "Left")) (v "Right"),
                                        "Left" #= tl (v "Left") ]
                                        (goto "loop"),

                        blja "do-write" ["Symbol" #= hd (tl (v "Instruction")), -- do-write: Symbol := hd(tl(tl("Instruction")));
                                         "Right" #= pl (v "Symbol") (tl (v "Right"))]  -- Right := cons(Symbol, tl("Right"));
                                        (goto "loop"), -- goto loop;

                        blja "do-goto" ["NextLabel" #= hd (tl (v "Instruction")), -- do-goto: NextLabel := hd(tl(tl(Instruction)));
                                        "QTail" #= drp (v "NextLabel") (v "Q")] -- "QTail" := newTail(Nextlabel, Q);
                                        (goto "loop"), -- goto loop;

                        blja "do-if" ["Symbol" #= hd (tl (v "Instruction")),  -- do-if: Symbol := hd(tl(tl(Instruction)));
                                     "NextLabel" #= hd (tl (tl (tl (v "Instruction"))))]  -- NextLabel := hd(tl(tl(tl(tl(Instruction)))));
                                      (if' (v "Symbol" ?= hd (v "Right") ) "jump" "loop"),  -- if Symbol == fistsym(Right) goto jump else loop;   

                        blja "jump" ["QTail" #= drp (v "NextLabel") (v "Q")]  -- "QTail" := newTail(Nextlabel, Q);
                             (goto "loop"),

                        blj "error" (Return (pl (EConstant $ s "Sytax error: ") (v "Instruction"))) ,

                        blj "stop" (Return (v "Right"))
                    ]
