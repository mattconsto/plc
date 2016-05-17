open Types
open Environment

exception Terminated of aquaTerm
exception StuckTerm of string
exception NonBaseTypeResult of aquaTerm

exception AssertionFailed of aquaTerm
exception LoopBreak
exception LoopContinue
exception Return of aquaTerm

val eval : out_channel -> out_channel -> in_channel -> aquaTerm environment -> aquaTerm -> aquaTerm
