open Types
open Environment
open Stringify

exception TypeError of string;;

(* The type checking function itself *)
let rec typeOf env e = flush_all(); match e with
	(* data *)
	| TermUnit        -> TypeUnit
	| TermNum       n -> TypeNum
	| TermPair (a, b) -> TypePair ((typeOf env a), (typeOf env b))
	| TermList      a -> TypeList (typeOf env (List.hd a))
	| TermVar       x -> (try lookup env x with EnvironmentReachedHead e -> raise (TypeError ("Cannot find a binding for variable " ^ x)))
	| TermString    s -> let rec exp i l = if i < 0 then l else exp (i - 1) (TypePair (TypeNum, l)) in exp (String.length s - 1) TypeUnit

	| TermReadInt | TermReadString | TermReadBool -> TypeNum
	| TermClear -> TypeUnit
	| TermPrintString a | TermPrintInt a | TermPrintBool a -> TypeUnit
	| TermErrorString a | TermErrorInt a | TermErrorBool a -> TypeUnit

	| TermRandom (a, b) -> TypeNum

	| TermEqual (a, b) -> (match (typeOf env a), (typeOf env b) with
		| TypeNum,  TypeNum  -> TypeNum
		| TypePair _, TypePair _ -> TypeNum
		| _ -> raise (TypeError ("Two Numbers/Pairs are required, given " ^ (type_to_string (typeOf env a)) ^ " and " ^ (type_to_string (typeOf env b)) ^ ".")))

	| TermLessThan (a, b) | TermLessThanEqual (a, b) | TermMoreThan (a, b) | TermMoreThanEqual (a, b) | TermNotEqual (a, b)
	-> (match (typeOf env a), (typeOf env b) with
		| TypeNum, TypeNum -> TypeNum
		| _ -> raise (TypeError ("Two Numbers are required, given " ^ (type_to_string (typeOf env a)) ^ " and " ^ (type_to_string (typeOf env b)) ^ ".")))

	| TermUnaryNot a -> (match (typeOf env a) with
		| TypeNum -> TypeNum
		| _ -> raise (TypeError ("A Number is required, given " ^ (type_to_string (typeOf env a)) ^ ".")))

	| TermStringLower a | TermStringUpper a | TermStringRev a -> (match (typeOf env a) with
		| TypePair(TypeNum, c) -> TypePair(TypeNum, c)
		| _ -> raise (TypeError ("A Pair is required, given " ^ (type_to_string (typeOf env a)) ^ ".")))

	| TermMathAbs a | TermMathSign a | TermMathSqrt a | TermMathLog a | TermMathLn a | TermMathFact a
	| TermUnaryMinus a | TermUnaryPlus a -> (match (typeOf env a) with
		| TypeNum -> TypeNum
		| _ -> raise (TypeError ("A Number is required, given " ^ (type_to_string (typeOf env a)) ^ ".")))

	| TermMathMin (a, b) | TermMathMax (a, b)
	| TermPower (a, b) | TermMultiply (a, b) | TermDivide (a, b) | TermModulo (a, b) | TermPlus (a, b) | TermSubtract (a, b)
	| TermShiftLeft (a, b) | TermShiftRight (a, b) | TermBitwiseAnd (a, b) | TermBitwiseXOr (a, b) | TermBitwiseOr (a, b)
	-> (match (typeOf env a), (typeOf env b) with
		| TypeNum, TypeNum -> TypeNum
		| _ -> raise (TypeError ("Two Numbers are required, given " ^ (type_to_string (typeOf env a)) ^ " and " ^ (type_to_string (typeOf env b)) ^ ".")))

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

	| TermAutoBind (x, e) -> (let temp = (typeOf (bind env x (typeOf env e)) e) in (ignore (bind env x temp); temp))

	| TermReBind (x, e) -> (match ((typeOf env e) = (lookup env x)) with
		| true  -> typeOf env e
		| false -> raise (TypeError ("You cannot rebind variable to a different type. Use <type> <ident> = <value> instead!")))

	| TermUnBind x -> lookup env x

	| TermLambda (x, old, t, e) -> TypeFun(t, typeOf (bind env x t) e)

	| TermApply (a, b) -> (	match typeOf env a with
		| TypeFun (tT, tU) -> (
 			match tT = typeOf env b with
				| true -> tU
				| false -> raise (TypeError "Function has a different type than expected")
				)
		| _ -> raise (TypeError (Printf.sprintf "While binding got %s which is not a function" (type_to_string (typeOf env a)))))

	| TermTypeOf a -> typeOf env (TermString (type_to_string (typeOf env a)))

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

	| TermLimit (l,n) -> (match (typeOf env l, typeOf env n) with
													| (TypePair(h,r),TypeNum) -> TypeList h
													| (TypeUnit,_) -> TypeUnit
													| (_,_) -> raise (TypeError "Limit"))
