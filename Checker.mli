open Types
open Environment

exception TypeError of string
exception FreeError

(* The type checking function itself *)
val typeOf : toyType environment -> toyTerm -> toyType
