open Types
open Environment
open Checker
open Evaluator

exception ParseError of string

let global_types  = extend Head
let global_values = extend Head

let parse str = try Parser.parser Lexer.lexer (Lexing.from_string str) with Parsing.Parse_error -> raise (ParseError str)
let check ele = typeOf global_types ele

let eval output error input ele = eval output error input global_values ele

(* Do everything *)
let run output error input str = try
  let ele = parse str in (
  ignore (check ele);
  eval output error input ele)
with
  | EnvironmentReachedHead -> Printf.fprintf error "Eval: Variable not bound!\n"; TermUnit
  | ParseError m           -> Printf.fprintf error "Parse: Failed to parse %s\n" m; TermUnit
  | TypeError m            -> Printf.fprintf error "Type: Bad type %s\n" m; TermUnit
  | Failure m              -> Printf.fprintf error "Failure: %s\n" m; TermUnit
  | End_of_file            -> TermUnit
  | Terminated m           -> m
  | StuckTerm m            -> Printf.fprintf error "Eval: Execution Stuck %s\n" m; TermUnit
  | NonBaseTypeResult      -> Printf.fprintf error "Eval: Non Base Type Result!\n"; TermUnit
  | AssertionFailed m      -> Printf.fprintf error "Eval: Assertion Failed %s\n" (result_to_bool m); TermUnit
  | LoopBreak              -> Printf.fprintf error "Eval: Break statement outside of loop\n"; TermUnit
  | LoopContinue           -> Printf.fprintf error "Eval: Continue statement outside of loop\n"; TermUnit
  | a                      -> Printf.fprintf error "Unknown %s\n" (Printexc.to_string a); TermUnit
