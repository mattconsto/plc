open Types
open Environment

exception UnboundVariableError of string;;
exception Terminated of string;;
exception StuckTerm of string;;
exception SubstitutionError;;
exception NonBaseTypeResult;;
exception AssertionFailed of toyTerm;;
exception LoopBreak;;
exception LoopContinue;;

val eval : toyTerm environment -> toyTerm -> toyTerm

val string_res : toyTerm -> string
val string_res_string : toyTerm -> string
