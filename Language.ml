open Types
open Environment
open Checker
open Evaluator

(* Extend the root environment Head, and type/eval the element given *)
let check element = typeOf (extend Head) element
let eval  element = eval   (extend Head) element
