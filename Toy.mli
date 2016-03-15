open Types;;

exception UnboundVariableError of string;;
exception Terminated of string;;
exception StuckTerm of string;;
exception SubstitutionError;;
exception NonBaseTypeResult;;
exception AssertionFailed of toyTerm;;

val typeProg  : toyTerm -> toyType
val evalProg  : toyTerm -> toyTerm
val string_res : toyTerm -> string
