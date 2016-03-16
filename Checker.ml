open Types
open Environment

exception TypeError of string;;

(* The type checking function itself *)
let rec typeOf env e = flush_all(); match e with
	(* data *)
	| TermUnit        -> TypeUnit
	| TermNum       n -> TypeNum
	| TermPair (a, b) -> TypePair ((typeOf env a), (typeOf env b))
	| TermList      a -> TypeList (typeOf env (List.hd a))
	| TermVar       x -> (try lookup env x with EnvironmentReachedHead -> raise (TypeError ("Variable: " ^ x)))
	| TermString    s -> TypePair (TypeNum, TypeNum)

	| TermReadInt     -> TypeNum
	| TermReadString  -> TypeNum
	| TermReadBool    -> TypeNum
	| TermPrintString a | TermPrintInt a | TermPrintBool a -> TypeUnit

	| TermRandom (a, b) -> TypeNum

	| TermLessThan (a, b) | TermLessThanEqual (a, b) | TermMoreThan (a, b) | TermMoreThanEqual (a, b) | TermEqual (a, b) | TermNotEqual (a, b)
	-> (match (typeOf env a), (typeOf env b) with
		| TypeNum, TypeNum -> TypeNum
		| _ -> raise (TypeError "Comparison"))

	| TermUnaryNot a -> (match (typeOf env a) with
		| TypeNum -> TypeNum
		| _ -> raise (TypeError "Unary Not"))

	| TermUnaryMinus a | TermUnaryPlus a -> (match (typeOf env a) with
		| TypeNum -> TypeNum
		| _ -> raise (TypeError "Unary Operation"))

	| TermPower (a, b) | TermMultiply (a, b) | TermDivide (a, b) | TermModulo (a, b) | TermPlus (a, b) | TermSubtract (a, b)
	| TermShiftLeft (a, b) | TermShiftRight (a, b) | TermBitwiseAnd (a, b) | TermBitwiseXOr (a, b) | TermBitwiseOr (a, b)
	-> (match (typeOf env a), (typeOf env b) with
		| TypeNum, TypeNum -> TypeNum
		| _ -> raise (TypeError "Binary Operation"))

	| TermScope a -> typeOf (extend env) a

	| TermWhile (a, b) | TermDo (a, b) -> let scope = extend env in (ignore (typeOf scope a); ignore (typeOf scope b)); TypeUnit
	| TermFor(a, b, c, d) -> let scope = extend env in (ignore (typeOf scope a); ignore (typeOf scope b); ignore (typeOf scope c); ignore (typeOf scope d)); TypeUnit
	| TermReturn a -> typeOf env a
	| TermAssert a -> TypeUnit
	| TermBreak -> TypeUnit
	| TermContinue -> TypeUnit
	| TermExit a -> TypeUnit

	| TermCons (a, b) -> (match typeOf env a, typeOf env b with n, m -> TypePair (n, m))
	| TermHead a -> (match typeOf env a with TypePair(j, k) -> j | _ -> raise (TypeError "Head"))
	| TermTail a -> (match typeOf env a with TypePair(j, k) -> k | _ -> raise (TypeError "Tail"))
	| TermLength a -> (match typeOf env a with TypePair(j,k) -> TypeNum | TypeUnit -> TypeNum | _ -> raise (TypeError "Length"))

	(* complicated *)
	| TermIf (a, b, c) -> let scope = extend env in (ignore (typeOf scope a); ignore (typeOf scope b); ignore (typeOf scope c)); TypeUnit

	| TermBind (x, t, e) -> (match ((typeOf env e) = t) with
		| true -> ignore (bind env x t); t
		| false -> raise (TypeError ("while binding " ^ x)))

	| TermReBind (x, e) -> (match ((typeOf env e) = (lookup env x)) with
		| true  -> typeOf env e
		| false -> raise (TypeError "Rebind"))

	| TermLambda (x, t, e) -> TypeFun(t, typeOf (bind env x t) e)

	| TermApply (a, b) -> (	match typeOf env a with
		| TypeFun (tT, tU) -> (
 			match tT = typeOf env b with
				| true -> tT
				| false -> raise (TypeError "Apply")
				)
		| _ -> raise (TypeError "Apply"))
