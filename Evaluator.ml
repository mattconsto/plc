open Types
open Environment

exception StuckTerm of string
exception NonBaseTypeResult

exception AssertionFailed of aquaTerm
exception LoopBreak
exception LoopContinue

let rec string_res res = match res with
	| TermUnit        -> "[]"
	| TermNum i       -> Printf.sprintf "%i" i
	| TermPair(a, b) -> (match b with
		| TermUnit -> (string_res a)
		| _      -> (string_res a) ^ " " ^ (string_res b))
	| TermLambda(x, t, a) -> Printf.sprintf "lambda (%s)" x
	| _ -> raise NonBaseTypeResult

let rec string_res_string res = match res with
	| TermUnit        -> ""
	| TermNum i       -> String.make 1 (Char.chr i)
	| TermPair(a, b) -> (match b with
		| TermUnit -> (string_res_string a)
		| _      -> (string_res_string a) ^ (string_res_string b))
	| TermLambda(x, t, a) -> Printf.sprintf "lambda (%s)" x 
	| _ -> raise NonBaseTypeResult

let equality_test e = (match e with
	| TermNum  0
	| TermUnit -> false
	| TermPair(_, _)
	| TermNum _ -> true
	| _ -> raise (StuckTerm "If"))

(* Read lines, caching the numbers found *)
let read_number_cache = ref [];;
let read_number = fun () -> (
	while !read_number_cache = [] do ignore (read_number_cache := Str.split (Str.regexp " ") (read_line ())) done;
	match !read_number_cache with
		| (head :: tail) -> (read_number_cache := tail; int_of_string head)
		| [] -> raise NonBaseTypeResult);;

(* bigEval *)
let rec eval env e = match e with
	| TermVar x -> lookup env x
	| TermNum n -> TermNum n

	| TermString (s)           -> eval env (
		let rec exp i l = if i < 0 then l else exp (i - 1) (TermCons (TermNum (Char.code s.[i]), l)) in
		exp (String.length s - 1) TermUnit)

	| TermRead n -> (match n with
		| TermNum a -> (let rec build a = if a = 0 then TermUnit else let left = read_number () and right = build (a-1) in TermPair(TermNum left, right) in build a)
		| _       -> raise (StuckTerm "Read"))

	| TermPrint a              -> print_string (string_res (eval env a) ^ "\n"); TermUnit
	| TermToString a             -> print_string (string_res_string (eval env a) ^ "\n"); TermUnit

	| TermLessThan      (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(if n <  m then 1 else 0)  | _ -> raise (StuckTerm "Less than"))
	| TermLessThanEqual (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(if n <= m then 1 else 0)  | _ -> raise (StuckTerm "Less than Equal"))
	| TermMoreThan      (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(if n >  m then 1 else 0)  | _ -> raise (StuckTerm "More than"))
	| TermMoreThanEqual (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(if n >= m then 1 else 0)  | _ -> raise (StuckTerm "More than Equal"))
	| TermEqual         (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(if n =  m then 1 else 0)  | _ -> raise (StuckTerm "Equal"))
	| TermNotEqual      (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(if n != m then 1 else 0)  | _ -> raise (StuckTerm "Not Equal"))

	| TermUnaryNot      (a)    -> (match eval env a with TermNum n -> TermNum(lnot n) | _ -> raise (StuckTerm "Unary Not"))
	| TermUnaryMinus    (a)    -> (match eval env a with TermNum n -> TermNum(-(n))   | _ -> raise (StuckTerm "Unary Minus"))
	| TermUnaryPlus     (a)    -> (match eval env a with TermNum n -> TermNum(+(n))   | _ -> raise (StuckTerm "Unary Plus"))

	| TermPower         (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(int_of_float (float_of_int n ** float_of_int m)) | _ -> raise (StuckTerm "Power"))
	| TermMultiply      (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(n * m)   | _ -> raise (StuckTerm "Multiply"))
	| TermDivide        (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(n / m)   | _ -> raise (StuckTerm "Divide"))
	| TermModulo        (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(n mod m) | _ -> raise (StuckTerm "Modulo"))
	| TermPlus          (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(n + m)   | _ -> raise (StuckTerm "Plus"))
	| TermSubtract      (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(n - m)   | _ -> raise (StuckTerm "Subtract"))

	| TermShiftLeft     (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(n lsl  m)  | _ -> raise (StuckTerm "Shift Left"))
	| TermShiftRight    (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(n lsr  m)  | _ -> raise (StuckTerm "Shift Right"))
	| TermBitwiseAnd    (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(n land m)  | _ -> raise (StuckTerm "Bitwise And"))
	| TermBitwiseXOr    (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(n lxor m)  | _ -> raise (StuckTerm "Bitwise XOr"))
	| TermBitwiseOr     (a, b) -> (match eval env a, eval env b with TermNum n, TermNum m -> TermNum(n lor  m)  | _ -> raise (StuckTerm "Bitwise Or"))

	| TermWhile         (p, a) -> (
		let scope = extend env in (
		try
			while equality_test (eval scope p) do (
				try
					ignore (eval scope a)
				with LoopContinue -> ()
			) done; TermUnit
		with LoopBreak -> TermUnit))
	| TermDo            (p, a) -> (
		let scope = extend env in (
		ignore (eval scope a);
		try
			while equality_test (eval scope p) do (
				try
					ignore (eval scope a)
				with LoopContinue -> ()
			) done; TermUnit
		with LoopBreak -> TermUnit))
	| TermFor     (a, b, c, d) -> (
		let scope = extend env in (
		ignore (eval scope a);
		try
			while equality_test (eval scope b) do (
				try
					ignore (eval scope c);
					ignore (eval scope d)
				with LoopContinue -> ()
			) done; TermUnit
		with LoopBreak -> TermUnit))
	| TermAssert (a)           -> (if not (equality_test (eval env a)) then raise (AssertionFailed a); TermUnit)
	| TermBreak                -> raise LoopBreak
	| TermContinue             -> raise LoopContinue

	| TermCons          (a, b) -> (match eval env a, eval env b with n, m -> TermPair(n, m))
	| TermHead          (a)    -> (match eval env a with TermPair (n, m) -> n | _ -> raise (StuckTerm "Head"))
	| TermTail          (a)    -> (match eval env a with TermPair (n, m) -> m | _ -> raise (StuckTerm "Tail"))

	| TermIf         (p, a, b) -> (let scope = extend env in match equality_test (eval scope p) with
		| false -> eval scope b
		| true  -> eval scope a)

	| TermBind (x, tT, a)         -> ignore (bind env x (eval env a)); TermUnit
	| TermReBind (x,  a)      -> ignore (rebind env x (eval env a)); TermUnit

  | TermApply (TermLambda(x, t, a), b) -> let scope = extend env in (ignore (bind scope x b); eval scope a)

	| TermLambda(x, t, a) -> TermLambda(x, t, a)

	| TermUnit -> TermUnit
	| _ -> raise (StuckTerm "Unknown")
