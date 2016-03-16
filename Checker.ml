open Types
open Environment

exception TypeError of string;;

(* The type checking function itself *)
let rec typeOf env e = match e with
	(* data *)
	| TermUnit        -> TypeUnit
	| TermNum       n -> TypeInt
	| TermPair (a, b) -> TypePair ((typeOf env a), (typeOf env b))
	| TermVar       x -> (try lookup env x with EnvironmentReachedHead -> raise (TypeError ("Variable: " ^ x)))
	| TermString    s -> TypePair (TypeInt, TypeInt)

	| TermRead e -> (match e with
		| TermNum n -> (let rec build n = if n = 0 then TypeUnit else TypePair(TypeInt, build (n-1)) in build n)
		| _       -> raise (TypeError "Read"))
	| TermToString a | TermPrint a -> TypeUnit

	| TermLessThan (a, b) | TermLessThanEqual (a, b) | TermMoreThan (a, b) | TermMoreThanEqual (a, b) | TermEqual (a, b) | TermNotEqual (a, b)
	-> (match (typeOf env a), (typeOf env b) with
		| TypeInt, TypeInt -> TypeInt
		| _ -> raise (TypeError "Comparison"))

	| TermUnaryNot a -> (match (typeOf env a) with
		| TypeInt -> TypeInt
		| _ -> raise (TypeError "Unary Not"))

	| TermUnaryMinus a | TermUnaryPlus a -> (match (typeOf env a) with
		| TypeInt -> TypeInt
		| _ -> raise (TypeError "Unary Operation"))

	| TermPower (a, b) | TermMultiply (a, b) | TermDivide (a, b) | TermModulo (a, b) | TermPlus (a, b) | TermSubtract (a, b)
	| TermShiftLeft (a, b) | TermShiftRight (a, b) | TermBitwiseAnd (a, b) | TermBitwiseXOr (a, b) | TermBitwiseOr (a, b)
	-> (match (typeOf env a), (typeOf env b) with
		| TypeInt, TypeInt -> TypeInt
		| _ -> raise (TypeError "Binary Operation"))

	| TermWhile (a, b) | TermDo (a, b) -> let scope = extend env in (ignore (typeOf scope a); ignore (typeOf scope b)); TypeUnit
	| TermFor(a, b, c, d) -> let scope = extend env in (ignore (typeOf scope a); ignore (typeOf scope b); ignore (typeOf scope c); ignore (typeOf scope d)); TypeUnit
	| TermAssert a -> TypeUnit
	| TermBreak -> TypeUnit
	| TermContinue -> TypeUnit

	| TermCons (a, b) -> (match typeOf env a, typeOf env b with n, m -> TypePair (n, m))
	| TermHead a -> (match typeOf env a with TypePair(j, k) -> j | _ -> raise (TypeError "Head"))
	| TermTail a -> (match typeOf env a with TypePair(j, k) -> k | _ -> raise (TypeError "Tail"))

	(* complicated *)
	| TermIf (a, b, c) -> let scope = extend env in (ignore (typeOf scope a); ignore (typeOf scope b); ignore (typeOf scope c)); TypeUnit

	| TermBind (x, t, e) -> (match ((typeOf env e) = t) with
		| true -> ignore (bind env x t); TypeUnit
		| false -> raise (TypeError "Let"))

	| TermReBind (x, e) -> (match ((typeOf env e) = (lookup env x)) with
		| true  -> TypeUnit
		| false -> raise (TypeError "Rebind"))

	| TermLambda (x, t, e) -> TypeFun(t, typeOf (bind env x t) e)

	| TermApply (a, b) -> (	match typeOf env a with
		| TypeFun (tT, tU) -> (
 			match tT = typeOf env b with
				| true -> tT
				| false -> raise (TypeError "Apply")
				)
		| _ -> raise (TypeError "Apply"))
