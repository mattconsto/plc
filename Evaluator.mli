open Types
open Environment

exception Terminated of aquaTerm
exception StuckTerm of string
exception NonBaseTypeResult

exception AssertionFailed of aquaTerm
exception LoopBreak
exception LoopContinue

val eval : aquaTerm environment -> aquaTerm -> aquaTerm

val result_to_int : aquaTerm -> string
val result_to_string : aquaTerm -> string
val result_to_bool : aquaTerm -> string
