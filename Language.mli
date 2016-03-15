open Types

(* Type check a tree *)
val check : toyTerm -> toyType
(* Evaluate a tree *)
val eval  : toyTerm -> toyTerm
