open Types
open Environment

exception Terminated of aquaTerm
exception StuckTerm of string
exception NonBaseTypeResult

exception AssertionFailed of aquaTerm
exception LoopBreak
exception LoopContinue
exception Return of aquaTerm

val eval : out_channel -> out_channel -> in_channel -> aquaTerm environment -> aquaTerm -> aquaTerm

val result_to_int : aquaTerm -> string
val result_to_string : aquaTerm -> string
val result_to_bool : aquaTerm -> string
