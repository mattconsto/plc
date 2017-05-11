open Types
open Environment
open Checker
open Evaluator
open Language
open Stringify;; (* ;; is required *)

if (Array.length Sys.argv) < 2 then (
	Printf.fprintf stdout "Interactive Mode:\n";
	while true do
		Printf.fprintf stdout "> ";
		Printf.fprintf stdout "= %s\n" (term_to_string (run stdout stdout stdin (read_line ())))
	done
) else (
	try (
		let load_file f =
		  let ic = open_in f in
		  let n = in_channel_length ic in
		  let s = String.create n in
		  really_input ic s 0 n;
		  close_in ic;
		  s in
		ignore (run stdout stderr stdin (load_file Sys.argv.(1))))
	with
		| Sys_error m -> Printf.fprintf stderr "%s\n" m
		| a           -> Printf.fprintf stderr "%s\n" (Printexc.to_string a))
