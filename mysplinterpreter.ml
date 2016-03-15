open Environment
open Toy
open Lexer
open Parser
open Arg
open Printf

exception ParseError of string;;

(* Parsing.set_trace true;; *)

let parseProgram str = try
	parser lexer (Lexing.from_string str)
with Parsing.Parse_error -> raise (ParseError str);;

let interactive = Array.length Sys.argv < 2;;
if interactive then print_string "Interactive Mode:\n";;

try
	if interactive then (
		while true do
			try
				print_string "> ";
				let parsedProg = parseProgram (read_line ()) in (
				ignore (typeProg parsedProg); (* Perform typechecking *)
				print_string ((string_res (evalProg parsedProg)) ^ "\n"))
			with
				| ParseError m           -> print_string ("Failed to parse: " ^ m ^ "\n")
				| EnvironmentReachedHead -> print_string "Variable not found!\n"
				| TypeError m            -> print_string ("Type error: " ^ m ^ "\n")
				| UnboundVariableError m -> print_string ("Variable not bound: " ^ m ^ "\n")
				| Terminated m           -> print_string ("Program Terminated: " ^ m ^ "\n")
				| StuckTerm m            -> print_string ("Execution Stuck: " ^ m ^ "\n")
				| NonBaseTypeResult      -> print_string "Non Base Type Result!\n"
				| AssertionFailed m      -> print_string "Assertion Failed!\n"
		done
	) else (
		let load_file f =
		  let ic = open_in f in
		  let n = in_channel_length ic in
		  let s = Bytes.create n in
		  really_input ic s 0 n;
		  close_in ic;
		  (s) in
		let parsedProg = parseProgram (load_file Sys.argv.(1)) in
		ignore (typeProg parsedProg); (* Perform typechecking *)
		ignore (evalProg parsedProg);
	)
with
	| End_of_file -> if interactive then print_string "Exiting"; exit 0
