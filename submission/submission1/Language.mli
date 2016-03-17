open Types

exception ParseError of string

(* Parse a syntax *)
val parse : string   -> aquaTerm
(* Type check a tree *)
val check : aquaTerm -> aquaType
(* Evaluate a tree *)
val eval  : out_channel -> out_channel -> in_channel -> aquaTerm -> aquaTerm
(* Run *)
val run   : out_channel -> out_channel -> in_channel -> string -> aquaTerm
