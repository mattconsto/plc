open Lexer
open Parser

open Types
open Environment
open Checker
open Evaluator

exception ParseError of string

let parse str = try Parser.parser Lexer.lexer (Lexing.from_string str) with Parsing.Parse_error -> raise (ParseError str)
let check ele = typeOf global_types ele

let eval co ce ci ele = eval co ce ci global_values ele

(* Do everything *)
let run co ce ci str = try
  let ele = parse str in (
    Random.self_init();
    ignore (check ele);
    eval co ce ci ele)
with
  | EnvironmentReachedHead m -> Printf.fprintf ce "Environment Error: Reached the top of the environment%s\n" m; TermUnit
  | SyntaxError m            -> Printf.fprintf ce "Syntax Error: %s\n" m; TermUnit
  | ParseError m             -> Printf.fprintf ce "Parse Error: Failed to parse %s\n" m; TermUnit
  | TypeError m              -> Printf.fprintf ce "Type Error: %s\n" m; TermUnit

  | End_of_file              -> TermUnit
  | Terminated m             -> m

  | StuckTerm m              -> Printf.fprintf ce "Eval Error: Execution Stuck: %s\n" m; TermUnit
  | NonBaseTypeResult m      -> Printf.fprintf ce "Eval Error: Non Base Type Result: %s\n" (term_to_string m); TermUnit
  | AssertionFailed m        -> Printf.fprintf ce "Eval Error: Assertion Failed: %s\n" (result_to_bool m); TermUnit
  | LoopBreak                -> Printf.fprintf ce "Eval Error: Break statement outside of loop\n"; TermUnit
  | LoopContinue             -> Printf.fprintf ce "Eval Error: Continue statement outside of loop\n"; TermUnit

  | Failure m                -> Printf.fprintf ce "Failure: %s\n" m; TermUnit
  | a                        -> Printf.fprintf ce "Unknown: %s\n" (Printexc.to_string a); TermUnit

(* Print AST *)
let ast co ce ci str = try
  let ele = parse str in (
    ignore (check ele);
    Printf.sprintf "%s\n" (term_to_string ele))
with
  | EnvironmentReachedHead m -> Printf.fprintf ce "Environment Error: Reached the top of the environment%s\n" m; ""
  | SyntaxError m            -> Printf.fprintf ce "Syntax Error: %s\n" m; ""
  | ParseError m             -> Printf.fprintf ce "Parse Error: Failed to parse %s\n" m; ""
  | TypeError m              -> Printf.fprintf ce "Type Error: %s\n" m; ""

  | End_of_file              -> ""
  | Terminated m             -> ""

  | Failure m                -> Printf.fprintf ce "Failure: %s\n" m; ""
  | a                        -> Printf.fprintf ce "Unknown: %s\n" (Printexc.to_string a); ""
