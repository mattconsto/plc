open Printf;;
open Environment;;

(* Types of the language *)
type toyType = ToyUnit | ToyInt | ToyPair of toyType * toyType | ToyFun of toyType * toyType

(* Grammar of the language *)
type toyTerm =
	| TmUnit
	| TmNum of int
	| TmPair of toyTerm * toyTerm
	| TmString of string

	| TmLessThan of toyTerm * toyTerm
	| TmLessThanEqual of toyTerm * toyTerm
	| TmMoreThan of toyTerm * toyTerm
	| TmMoreThanEqual of toyTerm * toyTerm
	| TmEqual of toyTerm * toyTerm
	| TmNotEqual of toyTerm * toyTerm

	| TmUnaryNot of toyTerm
	| TmUnaryMinus of toyTerm
	| TmUnaryPlus of toyTerm

	| TmPower of toyTerm * toyTerm
	| TmMultiply of toyTerm * toyTerm
	| TmDivide of toyTerm * toyTerm
	| TmModulo of toyTerm * toyTerm
	| TmPlus of toyTerm * toyTerm
	| TmSubtract of toyTerm * toyTerm

	| TmShiftLeft of toyTerm * toyTerm
	| TmShiftRight of toyTerm * toyTerm
	| TmBitwiseAnd of toyTerm * toyTerm
	| TmBitwiseXOr of toyTerm * toyTerm
	| TmBitwiseOr of toyTerm * toyTerm

	| TmWhile of toyTerm * toyTerm
	| TmDo of toyTerm * toyTerm
	| TmFor of toyTerm * toyTerm * toyTerm * toyTerm
	| TmBreak
	| TmContinue
	| TmAssert of toyTerm
	| TmRead of toyTerm
	| TmPrint of toyTerm
	| TmToString of toyTerm

	| TmCons of toyTerm * toyTerm
	| TmHead of toyTerm
	| TmTail of toyTerm

	| TmVar of string
	| TmIf of toyTerm * toyTerm * toyTerm
	| TmLet of string * toyType * toyTerm
	| TmReBind of string * toyTerm
	| TmAbs of string * toyType * toyTerm

exception TypeError of string;;
exception UnboundVariableError of string;;
exception Terminated of string;;
exception StuckTerm of string;;
exception FreeError;;
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

let global_type  = extend Head
let global_value = extend Head

(* The type checking function itself *)
let rec typeOf env e = match e with
	(* data *)
	| TmUnit     -> ToyUnit
	| TmNum  (n) -> ToyInt
	| TmPair (a, b) -> ToyPair ((typeOf env a), (typeOf env b))
	| TmVar  (x) -> (try lookup env x with EnvironmentReachedHead -> raise (TypeError ("Variable: " ^ x)))
	| TmString (s) -> ToyPair (ToyInt, ToyInt)

	| TmRead n -> (match n with
		| TmNum a -> (let rec build a = if a = 0 then ToyUnit else ToyPair(ToyInt, build (a-1)) in build a)
		| _       -> raise (TypeError "Read"))
	| TmToString a | TmPrint a -> ToyUnit

	(* int -> int -> bool *)
	| TmLessThan (a, b) | TmLessThanEqual (a, b) | TmMoreThan (a, b) | TmMoreThanEqual (a, b) | TmEqual (a, b) | TmNotEqual (a, b)
	-> (match (typeOf env a), (typeOf env b) with
		| ToyInt, ToyInt -> ToyInt
		| _ -> raise (TypeError "Comparison"))

	(* int/bool -> int/bool *)
	| TmUnaryNot (a)
	 -> (match (typeOf env a) with
		| ToyInt -> ToyInt
		| _ -> raise (TypeError "Unary Not"))

	(* int -> int *)
	| TmUnaryMinus (a) | TmUnaryPlus (a)
	-> (match (typeOf env a) with
		| ToyInt -> ToyInt
		| _ -> raise (TypeError "Unary Operation"))

	(* int -> int -> int *)
	| TmPower (a, b) | TmMultiply (a, b) | TmDivide (a, b) | TmModulo (a, b) | TmPlus (a, b) | TmSubtract (a, b)
	| TmShiftLeft (a, b) | TmShiftRight (a, b) | TmBitwiseAnd (a, b) | TmBitwiseXOr (a, b) | TmBitwiseOr (a, b)
	-> (match (typeOf env a), (typeOf env b) with
		| ToyInt, ToyInt -> ToyInt
		| _ -> raise (TypeError "Binary Operation"))

	| TmWhile (a, b) | TmDo (a, b) -> let scope = extend env in (ignore (typeOf scope a); ignore (typeOf scope b)); ToyUnit
	| TmFor(a, b, c, d) -> let scope = extend env in (ignore (typeOf scope a); ignore (typeOf scope b); ignore (typeOf scope c); ignore (typeOf scope d)); ToyUnit
	| TmAssert (a) -> ToyUnit
	| TmBreak -> ToyUnit
	| TmContinue -> ToyUnit

	| TmCons (a, b) -> (match typeOf env a, typeOf env b with n, m -> ToyPair (n, m))
	| TmHead (a) -> (match typeOf env a with ToyPair(j, k) -> j | _ -> raise (TypeError "Head"))
	| TmTail (a) -> (match typeOf env a with ToyPair(j, k) -> k | _ -> raise (TypeError "Tail"))

	(* complicated *)
	| TmIf (e1,e2,e3) -> let scope = extend env in (ignore (typeOf scope e1); ignore (typeOf scope e2); ignore (typeOf scope e3)); ToyUnit

	| TmLet (x, tT, e1) -> (match ((typeOf env e1) = tT) with
		| true -> ignore (bind env x tT); ToyUnit
		| false -> raise (TypeError "Let"))

	| TmReBind (x, e1) -> (match ((typeOf env e1) = (lookup env x)) with
		| true  ->  ToyUnit
		| false -> raise (TypeError "Rebind"))

	| TmAbs (x,tT,e) -> ignore (bind env x tT); ToyFun(tT, typeOf env e)

let rec type_to_string tT = match tT with
	| ToyUnit -> "unit"
	| ToyInt  -> "int"
	| ToyPair(a, ToyUnit) -> sprintf "[%s]" (type_to_string a)
	| ToyPair(a, b) -> sprintf "[%s.%s]" (type_to_string a) (type_to_string b)
	| ToyFun(tT1, tT2) -> "( "^type_to_string(tT1)^" -> "^type_to_string(tT2)^" )"
;;

let rec string_res res = match res with
	| TmUnit        -> "[]"
	| TmNum i       -> sprintf "%i" i
	| TmPair(a, b) -> match b with
		| TmUnit -> (string_res a)
		| _      -> (string_res a) ^ " " ^ (string_res b)
	| _ -> raise NonBaseTypeResult

let rec string_res_string res = match res with
	| TmUnit        -> ""
	| TmNum i       -> String.make 1 (Char.chr i)
	| TmPair(a, b) -> match b with
		| TmUnit -> (string_res_string a)
		| _      -> (string_res_string a) ^ (string_res_string b)
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
		| (head :: tail) -> (read_number_cache := tail; int_of_string head));;

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
;;

let typeProg e = typeOf global_type e;;
let evalProg e = eval global_value e;;
