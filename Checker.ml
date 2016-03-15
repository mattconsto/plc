open Types
open Environment

exception TypeError of string;;
exception FreeError;;

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
