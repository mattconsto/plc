open Types
open Environment

exception TypeError of string

(* The type checking function itself *)
val typeOf : aquaType environment -> aquaTerm -> aquaType
