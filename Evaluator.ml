open Types
open Environment

exception Terminated of aquaTerm
exception StuckTerm of string
exception NonBaseTypeResult of aquaTerm

exception AssertionFailed of aquaTerm
exception LoopBreak
exception LoopContinue
exception Return of aquaTerm

let rec result_to_int res = match res with
	| TermUnit        -> "[]"
	| TermNum i       -> Printf.sprintf "%i" i
	| TermPair(a, b) -> (match b with
		| TermUnit -> (result_to_int a)
		| _      -> (result_to_int a) ^ " " ^ (result_to_int b))
	| TermLambda(x, old, t, a) -> Printf.sprintf "lambda (%s)" x
	| TermList l -> (let rec printlist l s = match l with (h :: t) -> printlist t (s ^ " " ^ (result_to_int h)) | [] -> s in printlist l "")
	| TermLambda(x, env, t, a) -> Printf.sprintf "lambda (%s)" x
	| a -> raise (NonBaseTypeResult a)

let rec result_to_string res = match res with
	| TermUnit        -> ""
	| TermNum i       -> String.make 1 (Char.chr i)
	| TermPair(a, b) -> (match b with
		| TermUnit -> (result_to_string a)
		| _      -> (result_to_string a) ^ (result_to_string b))
	| TermLambda(x, env, t, a) -> Printf.sprintf "lambda (%s)" x
	| a -> raise (NonBaseTypeResult a)

let rec result_to_bool res = match res with
	| TermUnit        -> "[]"
	| TermNum i       -> if i != 0 then "true" else "false"
	| TermPair(a, b) -> (match b with
		| TermUnit -> (result_to_bool a)
		| _      -> (result_to_bool a) ^ " " ^ (result_to_bool b))
	| TermLambda(x, env, t, a) -> Printf.sprintf "lambda (%s)" x
	| a -> raise (NonBaseTypeResult a)

let equality_test e = (match e with
	| TermNum  0
	| TermUnit -> false
	| TermPair(_, _)
	| TermNum _ -> true
	| _ -> raise (StuckTerm "If"))

(* Read lines, caching the numbers found *)
let read_line_cache = ref [];;
let read_number input = fun () -> (
	while !read_line_cache = [] do ignore (read_line_cache := Str.split (Str.regexp " ") (input_line input)) done;
	match !read_line_cache with
		| (head :: tail) -> (read_line_cache := tail; int_of_string head)
		| [] -> raise (Failure "read_number"));;

let read_bool input = fun () -> (
	while !read_line_cache = [] do ignore (read_line_cache := Str.split (Str.regexp " ") (input_line input)) done;
	match !read_line_cache with
		| (head :: tail) -> (read_line_cache := tail; match String.lowercase head with
			| "true" | "t" | "yes" | "y" | "1" -> true
			| _ -> false)
		| [] -> raise (Failure "read_bool"));;

Random.self_init();;

(* bigEval *)
let rec eval output error input env e = flush_all (); match e with
	| TermVar x -> lookup env x
	| TermNum n -> TermNum n

	| TermList a -> TermList a

	| TermString (s)           -> eval output error input env (
		let rec exp i l = if i < 0 then l else exp (i - 1) (TermCons (TermNum (Char.code s.[i]), l)) in
		exp (String.length s - 1) TermUnit)

	| TermReadInt -> TermNum ((read_number input) ())
	| TermReadString -> TermNum 0
	| TermReadBool -> if (read_bool input) () then (eval output error input env (TermUnaryNot (TermNum 0))) else TermNum 0

	| TermPrintInt a              -> Printf.fprintf output "%s" (result_to_int (eval output error input env a)); TermUnit
	| TermPrintString a           -> Printf.fprintf output "%s" (result_to_string (eval output error input env a)); TermUnit
	| TermPrintBool a             -> Printf.fprintf output "%s" (result_to_bool (eval output error input env a)); TermUnit

	| TermRandom (TermNum min, TermNum max) -> TermNum (Random.int((max - min) + 1) + min)

	| TermLessThan      (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(if n <  m then 1 else 0)  | _ -> raise (StuckTerm "Less than"))
	| TermLessThanEqual (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(if n <= m then 1 else 0)  | _ -> raise (StuckTerm "Less than Equal"))
	| TermMoreThan      (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(if n >  m then 1 else 0)  | _ -> raise (StuckTerm "More than"))
	| TermMoreThanEqual (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(if n >= m then 1 else 0)  | _ -> raise (StuckTerm "More than Equal"))
	| TermEqual         (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(if n =  m then 1 else 0)  | _ -> raise (StuckTerm "Equal"))
	| TermNotEqual      (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(if n != m then 1 else 0)  | _ -> raise (StuckTerm "Not Equal"))

	| TermUnaryNot      (a)    -> (match eval output error input env a with TermNum n -> TermNum(lnot n) | _ -> raise (StuckTerm "Unary Not"))
	| TermUnaryMinus    (a)    -> (match eval output error input env a with TermNum n -> TermNum(-(n))   | _ -> raise (StuckTerm "Unary Minus"))
	| TermUnaryPlus     (a)    -> (match eval output error input env a with TermNum n -> TermNum(+(n))   | _ -> raise (StuckTerm "Unary Plus"))

	| TermPower         (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(int_of_float (float_of_int n ** float_of_int m)) | _ -> raise (StuckTerm "Power"))
	| TermMultiply      (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(n * m)   | _ -> raise (StuckTerm "Multiply"))
	| TermDivide        (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(n / m)   | _ -> raise (StuckTerm "Divide"))
	| TermModulo        (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(n mod m) | _ -> raise (StuckTerm "Modulo"))
	| TermPlus          (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(n + m)   | n, m -> raise (StuckTerm ("Plus")))
	| TermSubtract      (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(n - m)   | _ -> raise (StuckTerm "Subtract"))

	| TermShiftLeft     (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(n lsl  m)  | _ -> raise (StuckTerm "Shift Left"))
	| TermShiftRight    (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(n lsr  m)  | _ -> raise (StuckTerm "Shift Right"))
	| TermBitwiseAnd    (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(n land m)  | _ -> raise (StuckTerm "Bitwise And"))
	| TermBitwiseXOr    (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(n lxor m)  | _ -> raise (StuckTerm "Bitwise XOr"))
	| TermBitwiseOr     (a, b) -> (match eval output error input env a, eval output error input env b with TermNum n, TermNum m -> TermNum(n lor  m)  | _ -> raise (StuckTerm "Bitwise Or"))

	| TermScope             a  ->	eval output error input (extend env) a

	| TermWhile         (p, a) -> (
		let scope = extend env in (
		try
			while equality_test (eval output error input scope p) do (
				try
					ignore (eval output error input scope a)
				with LoopContinue -> ()
			) done; TermUnit
		with
			| Return a  -> a
			| LoopBreak -> TermUnit))
	| TermDo            (p, a) -> (
		let scope = extend env in (
		ignore (eval output error input scope a);
		try
			while equality_test (eval output error input scope p) do (
				try
					ignore (eval output error input scope a)
				with LoopContinue -> ()
			) done; TermUnit
		with
			| Return a  -> a
			| LoopBreak -> TermUnit))
	| TermFor     (a, b, c, d) -> (
		let scope = extend env in (
		ignore (eval output error input scope a);
		try
			while equality_test (eval output error input scope b) do (
				try
					ignore (eval output error input scope c);
					ignore (eval output error input scope d)
				with LoopContinue -> ()
			) done; TermUnit
		with
			| Return a  -> a
			| LoopBreak -> TermUnit))
	| TermReturn a             -> raise (Return a)
	| TermAssert (a)           -> (if not (equality_test (eval output error input env a)) then raise (AssertionFailed a); TermUnit)
	| TermBreak                -> raise LoopBreak
	| TermContinue             -> raise LoopContinue
	| TermExit a               -> raise (Terminated (eval output error input env a))

	| TermCons          (a, b) -> (match eval output error input env a, eval output error input env b with n, m -> TermPair(n, m))
	| TermConsFirst     (a, b) -> (match eval output error input env a, eval output error input env b with n, m -> n)
	| TermConsLast      (a, b) -> (match eval output error input env a, eval output error input env b with n, m -> m)
	| TermHead          (a)    -> (match eval output error input env a with TermPair (n, m) -> n | _ -> raise (StuckTerm "Head"))
	| TermTail          (a)    -> (match eval output error input env a with TermPair (n, m) -> m | _ -> raise (StuckTerm "Tail"))
	| TermLength 				(a)		 -> (let rec length num l = match l with
																	| TermPair (n, m) -> length (num + 1) m
																	| TermUnit				-> num
																	| _								-> raise (StuckTerm "Length") in
																TermNum (length 0 (eval output error input env a)))

	| TermIf         (p, a, b) -> (let scope = extend env in match equality_test (eval output error input scope p) with
		| false -> eval output error input scope b
		| true  -> eval output error input scope a)

	| TermBind (x, tT, a)     -> let temp = eval output error input (bind env x a) a in (ignore (rebind env x temp); temp)
	| TermReBind (x,  a)      -> let temp = eval output error input env a in (ignore (rebind env x temp); temp)

	| TermApply (a, b) -> (match (eval output error input env a) with
		| TermLambda(x, env, t, a) -> (try
				let scope = extend env in (ignore (bind scope x (eval output error input scope b)); eval output error input scope a)
			with Return a -> a)
		| _ -> raise (StuckTerm "Apply"))

	| TermLambda(x, old, t, a) -> TermLambda(x, env, t, a)

	| TermMap (f,d) -> (let rec map func data result =
												(match data with
													| TermPair (head,tail) -> let apply = (TermApply (func, head)) in
																										let r = (eval output error input env apply) in
																										map func tail (r :: result)
													| TermUnit			-> TermList result
													| _ 						-> raise (StuckTerm "Map")) in
											map (eval output error input env f) (eval output error input env d) [])

	| TermUnit -> TermUnit
	| _ -> raise (StuckTerm "Unknown")
