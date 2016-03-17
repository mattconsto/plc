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
	| TermVar       x -> (try lookup env x with EnvironmentReachedHead e -> raise (TypeError ("Variable: " ^ x)))
	| TermString    s -> TypePair (TypeNum, TypeNum)

	| TermReadInt | TermReadString | TermReadBool -> TypeNum
	| TermClear -> TypeUnit
	| TermPrintString a | TermPrintInt a | TermPrintBool a -> TypeUnit
	| TermErrorString a | TermErrorInt a | TermErrorBool a -> TypeUnit

	| TermRandom (a, b) -> TypeNum

	| TermLessThan (a, b) | TermLessThanEqual (a, b) | TermMoreThan (a, b) | TermMoreThanEqual (a, b) | TermEqual (a, b) | TermNotEqual (a, b)
	-> (match (typeOf env a), (typeOf env b) with
		| TypeNum, TypeNum -> TypeNum
		| _ -> raise (TypeError "Comparison"))

	| TermUnaryNot a -> (match (typeOf env a) with
		| TypeNum -> TypeNum
		| _ -> raise (TypeError "Unary Not"))

	| TermMathAbs a | TermMathSign a | TermMathSqrt a | TermMathLog a | TermMathLn a | TermMathFact a
	| TermUnaryMinus a | TermUnaryPlus a -> (match (typeOf env a) with
		| TypeNum -> TypeNum
		| _ -> raise (TypeError "Unary Operation"))

	| TermMathMin (a, b) | TermMathMax (a, b)
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
	| TermConsFirst (a, b) -> (match typeOf env a, typeOf env b with n, m -> n)
	| TermConsLast (a, b) -> (match typeOf env a, typeOf env b with n, m -> m)
	| TermHead a -> (match typeOf env a with TypePair(j, k) -> j | _ -> raise (TypeError "Head"))
	| TermTail a -> (match typeOf env a with TypePair(j, k) -> k | _ -> raise (TypeError "Tail"))
	| TermLength a -> (match typeOf env a with TypePair(j,k) -> TypeNum | TypeUnit -> TypeNum | _ -> raise (TypeError "Length"))

	(* complicated *)
	| TermIf (a, b, c) -> let scope = extend env in (ignore (typeOf scope a); ignore (typeOf scope b); ignore (typeOf scope c)); TypeUnit

	| TermBind (x, t, e) -> (let temp = (typeOf (bind env x t) e) in match (temp = t) with
		| true -> ignore (bind env x t); t
		| false -> raise (TypeError (Printf.sprintf "While binding got %s, expected %s" (type_to_string temp) (type_to_string t))))

	| TermReBind (x, e) -> (match ((typeOf env e) = (lookup env x)) with
		| true  -> typeOf env e
		| false -> raise (TypeError "Rebind"))

	| TermUnBind x -> lookup env x

	| TermLambda (x, old, t, e) -> TypeFun(t, typeOf (bind env x t) e)

	| TermApply (a, b) -> (	match typeOf env a with
		| TypeFun (tT, tU) -> (
 			match tT = typeOf env b with
				| true -> tT
				| false -> raise (TypeError "Apply")
				)
		| _ -> raise (TypeError (Printf.sprintf "While binding got %s which is not a function" (type_to_string (typeOf env a)))))

	| TermMap (f, l) -> ( match (f, typeOf env l) with
												| (TermLambda (x,old,t,e), TypePair (h,r)) -> TypeList h
												| (TermLambda (x,old,t,e), TypeUnit) 			 -> TypeUnit
												| (_, _) 															 -> raise (TypeError "Map"))

	| TermFilter (f, l) -> (match (f, typeOf env l) with
													| (TermLambda (x,old,t,e), TypePair(h,r)) -> TypeList h
													| (TermLambda (x,old,t,e), TypeUnit)			-> TypeUnit
													| (_,_)																		-> raise (TypeError "Filter"))

	| TermFold (f, l, n) -> (match (f, typeOf env l, typeOf env n) with
														| (TermLambda(x,old,t,e), TypePair(h,r), TypeNum) -> TypeList h
														| (TermLambda(x,old,t,e), TypeUnit, TypeNum) -> TypeUnit
														| (_,_,_) 																					-> raise (TypeError "Fold"))
