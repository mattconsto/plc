open Types
open Environment
open Checker
open Evaluator
open Language;; (* ;; is required *)

try
	if (Array.length Sys.argv) < 2 then (
		print_string "Interactive Mode:\n";
		while true do
			try
				print_string "> ";
				let parsedProg = parse (read_line ()) in (
				ignore (check parsedProg); (* Perform typechecking *)
				print_string ((result_to_int (eval parsedProg)) ^ "\n"))
			with
				| EnvironmentReachedHead -> print_string "Variable not bound!\n"
				| ParseError m           -> print_string ("Failed to parse: " ^ m ^ "\n")
				| TypeError m            -> print_string ("Bad type: " ^ m ^ "\n")
				| Failure m              -> print_string ("Failure: " ^ m ^ "\n")
				| Terminated m           -> print_string ("Evaluation Terminated: " ^ (result_to_int m) ^"\n")
				| StuckTerm m            -> print_string ("Execution Stuck: " ^ m ^ "\n")
				| NonBaseTypeResult      -> print_string "Non Base Type Result!\n"
				| AssertionFailed m      -> print_string ("Assertion Failed: " ^ (result_to_bool m) ^ "\n")
				| LoopBreak              -> print_string "Break statement outside of loop\n"
				| LoopContinue           -> print_string "Continue statement outside of loop\n"
				| _                      -> print_string "Unknown Exception!\n"
		done
	) else (
		let load_file f =
		  let ic = open_in f in
		  let n = in_channel_length ic in
		  let s = Bytes.create n in
		  really_input ic s 0 n;
		  close_in ic;
		  s in
		let parsedProg = parse (load_file Sys.argv.(1)) in
		ignore (check parsedProg); (* Perform typechecking *)
		ignore (eval parsedProg);
	)
with End_of_file -> exit 0
