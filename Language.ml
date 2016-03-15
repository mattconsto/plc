open Types
open Environment
open Checker
open Evaluator

exception ParseError of string

let parse str = try Parser.parser Lexer.lexer (Lexing.from_string str) with Parsing.Parse_error -> raise (ParseError str)
let check ele = typeOf (extend Head) ele
let eval  ele = eval   (extend Head) ele
