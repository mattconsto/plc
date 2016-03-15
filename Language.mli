open Types

exception ParseError of string

(* Parse a syntax *)
val parse : string   -> aquaTerm
(* Type check a tree *)
val check : aquaTerm -> aquaType
(* Evaluate a tree *)
val eval  : aquaTerm -> aquaTerm
