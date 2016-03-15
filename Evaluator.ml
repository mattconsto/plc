open Types
open Environment

exception UnboundVariableError of string;;
exception Terminated of string;;
exception StuckTerm of string;;
exception SubstitutionError;;
exception NonBaseTypeResult;;
exception AssertionFailed of toyTerm;;
exception LoopBreak;;
exception LoopContinue;;

let rec isValue e = match e with
	| TmNum n        -> true
	| TmAbs(x,tT,e') -> true
	| _              -> false
;;

let rec string_res res = match res with
	| TmUnit        -> "[]"
	| TmNum i       -> Printf.sprintf "%i" i
	| TmPair(a, b) -> (match b with
		| TmUnit -> (string_res a)
		| _      -> (string_res a) ^ " " ^ (string_res b))
	| _ -> raise NonBaseTypeResult

let rec string_res_string res = match res with
	| TmUnit        -> ""
	| TmNum i       -> String.make 1 (Char.chr i)
	| TmPair(a, b) -> (match b with
		| TmUnit -> (string_res_string a)
		| _      -> (string_res_string a) ^ (string_res_string b))
	| _ -> raise NonBaseTypeResult

let eval_equals e = (match e with
	| TmNum  0
	| TmUnit -> false
	| TmPair(_, _)
	| TmNum _ -> true
	| _ -> raise (StuckTerm "If"))

let eval_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (TmCons (TmNum (Char.code s.[i]), l)) in
  exp (String.length s - 1) TmUnit

let read_number_cache = ref [];;
let read_number = fun () -> (
	if !read_number_cache = [] then	ignore (read_number_cache := Str.split (Str.regexp " ") (read_line ()));
	match !read_number_cache with
		| (head :: tail) -> (read_number_cache := tail; int_of_string head)
		| [] -> raise LoopContinue);;

(* bigEval *)
let rec eval env e = match e with
	| TmVar x -> lookup env x
	| e when isValue e -> e

	| TmString (s)           -> eval env (eval_string s)

	| TmRead n -> (match n with
		| TmNum a -> (let rec build a = if a = 0 then TmUnit else let left = read_number () and right = build (a-1) in TmPair(TmNum left, right) in build a)
		| _       -> raise (StuckTerm "Read"))

	| TmPrint a              -> print_string (string_res (eval env a) ^ "\n"); TmUnit
	| TmToString a             -> print_string (string_res_string (eval env a) ^ "\n"); TmUnit

	| TmLessThan      (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(if n <  m then 1 else 0)  | _ -> raise (StuckTerm "Less than"))
	| TmLessThanEqual (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(if n <= m then 1 else 0)  | _ -> raise (StuckTerm "Less than Equal"))
	| TmMoreThan      (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(if n >  m then 1 else 0)  | _ -> raise (StuckTerm "More than"))
	| TmMoreThanEqual (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(if n >= m then 1 else 0)  | _ -> raise (StuckTerm "More than Equal"))
	| TmEqual         (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(if n =  m then 1 else 0)  | _ -> raise (StuckTerm "Equal"))
	| TmNotEqual      (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(if n != m then 1 else 0)  | _ -> raise (StuckTerm "Not Equal"))

	| TmUnaryNot      (a)    -> (match eval env a with TmNum n -> TmNum(lnot n) | _ -> raise (StuckTerm "Unary Not"))
	| TmUnaryMinus    (a)    -> (match eval env a with TmNum n -> TmNum(-(n))   | _ -> raise (StuckTerm "Unary Minus"))
	| TmUnaryPlus     (a)    -> (match eval env a with TmNum n -> TmNum(+(n))   | _ -> raise (StuckTerm "Unary Plus"))

	| TmPower         (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(int_of_float (float_of_int n ** float_of_int m)) | _ -> raise (StuckTerm "Power"))
	| TmMultiply      (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(n * m)   | _ -> raise (StuckTerm "Multiply"))
	| TmDivide        (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(n / m)   | _ -> raise (StuckTerm "Divide"))
	| TmModulo        (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(n mod m) | _ -> raise (StuckTerm "Modulo"))
	| TmPlus          (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(n + m)   | _ -> raise (StuckTerm "Plus"))
	| TmSubtract      (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(n - m)   | _ -> raise (StuckTerm "Subtract"))

	| TmShiftLeft     (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(n lsl  m)  | _ -> raise (StuckTerm "Shift Left"))
	| TmShiftRight    (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(n lsr  m)  | _ -> raise (StuckTerm "Shift Right"))
	| TmBitwiseAnd    (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(n land m)  | _ -> raise (StuckTerm "Bitwise And"))
	| TmBitwiseXOr    (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(n lxor m)  | _ -> raise (StuckTerm "Bitwise XOr"))
	| TmBitwiseOr     (a, b) -> (match eval env a, eval env b with TmNum n, TmNum m -> TmNum(n lor  m)  | _ -> raise (StuckTerm "Bitwise Or"))

	| TmWhile         (p, a) -> (
		let scope = extend env in (
		try
			while eval_equals (eval scope p) do (
				try
					ignore (eval scope a)
				with LoopContinue -> ()
			) done; TmUnit
		with LoopBreak -> TmUnit))
	| TmDo            (p, a) -> (
		let scope = extend env in (
		ignore (eval scope a);
		try
			while eval_equals (eval scope p) do (
				try
					ignore (eval scope a)
				with LoopContinue -> ()
			) done; TmUnit
		with LoopBreak -> TmUnit))
	| TmFor     (a, b, c, d) -> (
		let scope = extend env in (
		ignore (eval scope a);
		try
			while eval_equals (eval scope b) do (
				try
					ignore (eval scope c);
					ignore (eval scope d)
				with LoopContinue -> ()
			) done; TmUnit
		with LoopBreak -> TmUnit))
	| TmAssert (a)           -> (if not (eval_equals (eval env a)) then raise (AssertionFailed a); TmUnit)
	| TmBreak                -> raise LoopBreak
	| TmContinue             -> raise LoopContinue

	| TmCons          (a, b) -> (match eval env a, eval env b with n, m -> TmPair(n, m))
	| TmHead          (a)    -> (match eval env a with TmPair (n, m) -> n | _ -> raise (StuckTerm "Head"))
	| TmTail          (a)    -> (match eval env a with TmPair (n, m) -> m | _ -> raise (StuckTerm "Tail"))

	| TmIf         (p, a, b) -> (let scope = extend env in match eval_equals (eval scope p) with
		| false -> eval scope b
		| true  -> eval scope a)

	| TmLet (x, tT, a)         -> ignore (bind env x (eval env a)); TmUnit
	| TmReBind (x,  a)      -> ignore (rebind env x (eval env a)); TmUnit

	| TmUnit -> TmUnit
	| _ -> raise (StuckTerm "Unknown")
