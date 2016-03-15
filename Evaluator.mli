open Types
open Environment

exception StuckTerm of string
exception NonBaseTypeResult

exception AssertionFailed of aquaTerm
exception LoopBreak
exception LoopContinue

val eval : aquaTerm environment -> aquaTerm -> aquaTerm

val string_res : aquaTerm -> string
val string_res_string : aquaTerm -> string
