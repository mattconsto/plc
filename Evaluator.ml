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
	| TermList l -> (let rec printlist l s = match l with (h :: t) -> printlist t (s ^ " " ^ (result_to_int h)) | [] -> s in printlist l "")
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
		| [] -> raise (Failure "Unable to read number, end of file?"));;

let read_bool input = fun () -> (
	while !read_line_cache = [] do ignore (read_line_cache := Str.split (Str.regexp " ") (input_line input)) done;
	match !read_line_cache with
		| (head :: tail) -> (read_line_cache := tail; match String.lowercase head with
			| "true" | "t" | "yes" | "y" | "1" -> true
			| _ -> false)
		| [] -> raise (Failure "Unable to read boolean, end of file?"));;

(* bigEval *)
let rec eval co ce ci env e = flush_all (); match e with
	| TermVar            x     -> lookup env x
	| TermNum            n     -> TermNum n
	| TermList           a     -> TermList a

	| TermString         s     -> eval co ce ci env (let rec exp i l = if i < 0 then l else exp (i - 1) (TermCons (TermNum (Char.code s.[i]), l)) in exp (String.length s - 1) TermUnit)

	| TermReadInt              -> TermNum ((read_number ci) ())
	| TermReadString           -> (eval co ce ci env (TermString (input_line ci)))
	| TermReadBool             -> if (read_bool ci) () then (eval co ce ci env (TermUnaryNot (TermNum 0))) else TermNum 0

	| TermClear                -> ignore (Sys.command "clear"); TermUnit

	| TermPrintInt       a     -> Printf.fprintf co "%s" (result_to_int (eval co ce ci env a)); TermUnit
	| TermPrintString    a     -> Printf.fprintf co "%s" (result_to_string (eval co ce ci env a)); TermUnit
	| TermPrintBool      a     -> Printf.fprintf co "%s" (result_to_bool (eval co ce ci env a)); TermUnit

	| TermErrorInt       a     -> Printf.fprintf ce "%s" (result_to_int (eval co ce ci env a)); TermUnit
	| TermErrorString    a     -> Printf.fprintf ce "%s" (result_to_string (eval co ce ci env a)); TermUnit
	| TermErrorBool      a     -> Printf.fprintf ce "%s" (result_to_bool (eval co ce ci env a)); TermUnit

	| TermRandom        (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum min, TermNum max -> TermNum (Random.int((max - min) + 1) + min)| _ -> raise (StuckTerm "Random"))

	| TermLessThan      (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(if n <  m then -1 else 0) | _ -> raise (StuckTerm "Less than"))
	| TermLessThanEqual (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(if n <= m then -1 else 0) | _ -> raise (StuckTerm "Less than Equal"))
	| TermMoreThan      (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(if n >  m then -1 else 0) | _ -> raise (StuckTerm "More than"))
	| TermMoreThanEqual (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(if n >= m then -1 else 0) | _ -> raise (StuckTerm "More than Equal"))
	| TermEqual         (a, b) -> (match eval co ce ci env a, eval co ce ci env b with
		| TermNum  n, TermNum  m -> TermNum(if n =  m then -1 else 0)
		| TermPair (n1, n2), TermPair (m1, m2) -> let rec pairs_equal j k = (match eval co ce ci env j, eval co ce ci env k with
				| TermPair(TermNum o, TermNum p), TermPair(TermNum q, TermNum r) -> o == q && p == r
				| TermPair(TermNum o, TermUnit),  TermPair(TermNum q, TermUnit)  -> o == q
				| TermPair(TermUnit,  TermNum p), TermPair(TermUnit,  TermNum r) -> p == r
				| TermPair(TermNum o, p),         TermPair(TermNum q, r)         -> o == q && (pairs_equal p r)
				| TermPair(TermUnit,  p),         TermPair(TermUnit,  r)         -> pairs_equal p r
				| TermPair(o, TermNum p),         TermPair(q, TermNum r)         -> (pairs_equal o q) && p == r
				| TermPair(o, TermUnit),          TermPair(q, TermUnit)          -> (pairs_equal o q)
				| TermPair(o, p), TermPair(q, r)                                 -> (pairs_equal o p) && (pairs_equal q r)
				| y, z                                                           -> false)
			in TermNum (if pairs_equal (TermPair (n1, n2)) (TermPair (m1, m2)) then -1 else 0)
		| _ -> raise (StuckTerm "Equal"))
	| TermNotEqual      (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(if n != m then -1 else 0) | _ -> raise (StuckTerm "Not Equal"))

	| TermUnaryNot       a     -> (match eval co ce ci env a with TermNum n -> TermNum(lnot n) | _ -> raise (StuckTerm "Unary Not"))
	| TermUnaryMinus     a     -> (match eval co ce ci env a with TermNum n -> TermNum(-(n))   | _ -> raise (StuckTerm "Unary Minus"))
	| TermUnaryPlus      a     -> (match eval co ce ci env a with TermNum n -> TermNum(+(n))   | _ -> raise (StuckTerm "Unary Plus"))

	| TermPower         (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(int_of_float (float n ** float m)) | _ -> raise (StuckTerm "Power"))
	| TermMultiply      (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(n * m)   | _ -> raise (StuckTerm "Multiply"))
	| TermDivide        (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(n / m)   | _ -> raise (StuckTerm "Divide"))
	| TermModulo        (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(n mod m) | _ -> raise (StuckTerm "Modulo"))
	| TermPlus          (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(n + m)   | n, m -> raise (StuckTerm ("Plus")))
	| TermSubtract      (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(n - m)   | _ -> raise (StuckTerm "Subtract"))

	| TermStringLower    a     -> (let rec convert s acc = match s with
		| TermUnit                -> acc
		| TermPair (TermNum n, o) -> convert o (TermPair (TermNum (Char.code (Char.lowercase (Char.chr n))), acc))
		| a                       -> raise (NonBaseTypeResult a)
	in convert (eval co ce ci env a) TermUnit)
	| TermStringUpper    a     -> (let rec convert s acc = match s with
		| TermUnit                -> acc
		| TermPair (TermNum n, o) -> convert o (TermPair (TermNum (Char.code (Char.uppercase (Char.chr n))), acc))
		| a                       -> raise (NonBaseTypeResult a)
	in convert (eval co ce ci env a) TermUnit)
	| TermStringRev    a     -> (let rec convert s acc = match s with
		| TermUnit                -> acc
		| TermPair (TermNum n, o) -> convert o (TermPair ((TermNum n), acc))
		| a                       -> raise (NonBaseTypeResult a)
	in convert (eval co ce ci env a) TermUnit)

	| TermMathMin       (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(if n < m then n else m) | _ -> raise (StuckTerm "Min"))
	| TermMathMax       (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(if n > m then n else m) | _ -> raise (StuckTerm "Max"))
	| TermMathAbs        a     -> (match eval co ce ci env a with TermNum n -> TermNum(abs n) | _ -> raise (StuckTerm "Abs"))
	| TermMathSign       a     -> (match eval co ce ci env a with TermNum n -> TermNum(if n > 0 then 1 else if n < 0 then -1 else 0) | _ -> raise (StuckTerm "Sign"))
	| TermMathSqrt       a     -> (match eval co ce ci env a with TermNum n -> TermNum(int_of_float (sqrt (float n))) | _ -> raise (StuckTerm "Sqrt"))
	| TermMathLog        a     -> (match eval co ce ci env a with TermNum n -> TermNum(int_of_float (log10 (float n))) | _ -> raise (StuckTerm "Log"))
	| TermMathLn         a     -> (match eval co ce ci env a with TermNum n -> TermNum(int_of_float (log (float n))) | _ -> raise (StuckTerm "Ln"))
	| TermMathFact       a     -> (match eval co ce ci env a with TermNum n -> (let rec fact x = if x <= 1 then 1 else x * fact (x - 1) in TermNum (fact n)) | _ -> raise (StuckTerm "Fact"))

	| TermShiftLeft     (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(n lsl  m)  | _ -> raise (StuckTerm "Shift Left"))
	| TermShiftRight    (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(n lsr  m)  | _ -> raise (StuckTerm "Shift Right"))
	| TermBitwiseAnd    (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(n land m)  | _ -> raise (StuckTerm "Bitwise And"))
	| TermBitwiseXOr    (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(n lxor m)  | _ -> raise (StuckTerm "Bitwise XOr"))
	| TermBitwiseOr     (a, b) -> (match eval co ce ci env a, eval co ce ci env b with TermNum n, TermNum m -> TermNum(n lor  m)  | _ -> raise (StuckTerm "Bitwise Or"))

	| TermScope             a  ->	eval co ce ci (extend env) a

	| TermWhile         (p, a) -> (
		let scope = extend env in (
		try
			while equality_test (eval co ce ci scope p) do (
				try
					ignore (eval co ce ci scope a)
				with LoopContinue -> ()
			) done; TermUnit
		with
			| Return a  -> a
			| LoopBreak -> TermUnit))
	| TermDo            (p, a) -> (
		let scope = extend env in (
		ignore (eval co ce ci scope a);
		try
			while equality_test (eval co ce ci scope p) do (
				try
					ignore (eval co ce ci scope a)
				with LoopContinue -> ()
			) done; TermUnit
		with
			| Return a  -> a
			| LoopBreak -> TermUnit))
	| TermFor     (a, b, c, d) -> (
		let scope = extend env in (
		ignore (eval co ce ci scope a);
		try
			while equality_test (eval co ce ci scope b) do (
				try
					ignore (eval co ce ci scope c);
					ignore (eval co ce ci scope d)
				with LoopContinue -> ()
			) done; TermUnit
		with
			| Return a  -> a
			| LoopBreak -> TermUnit))
	| TermReturn         a     -> raise (Return a)
	| TermAssert         p     -> (if not (equality_test (eval co ce ci env p)) then raise (AssertionFailed p); TermUnit)
	| TermBreak                -> raise LoopBreak
	| TermContinue             -> raise LoopContinue
	| TermExit           p     -> raise (Terminated (eval co ce ci env p))

	| TermPair          (a, b) -> TermPair(a, b)
	| TermCons          (a, b) -> (match eval co ce ci env a, eval co ce ci env b with n, m -> TermPair(n, m))
	| TermConsFirst     (a, b) -> (match eval co ce ci env a, eval co ce ci env b with n, m -> n)
	| TermConsLast      (a, b) -> (match eval co ce ci env a, eval co ce ci env b with n, m -> m)
	| TermHead           a     -> (match eval co ce ci env a with TermPair (n, m) -> n | _ -> raise (StuckTerm "Head"))
	| TermTail           a     -> (match eval co ce ci env a with TermPair (n, m) -> m | _ -> raise (StuckTerm "Tail"))
	| TermLength 				 a 		 -> (let rec length num l = match l with
																	| TermPair (n, m) -> length (num + 1) m
																	| TermUnit				-> num
																	| _								-> raise (StuckTerm "Length") in
																TermNum (length 0 (eval co ce ci env a)))

	| TermIf         (p, a, b) -> (let scope = extend env in match equality_test (eval co ce ci scope p) with
		| false -> eval co ce ci scope b
		| true  -> eval co ce ci scope a)

	| TermBind       (x, t, a) -> let temp = eval co ce ci (bind env x a) a in (ignore (rebind env x temp); temp)
	| TermReBind        (x, a) -> let temp = eval co ce ci env a in (ignore (rebind env x temp); temp)
	| TermUnBind         x     -> let temp = lookup env x in (ignore (unbind env x); temp)

	| TermApply         (a, b) -> (match (eval co ce ci env a) with
		| TermLambda(x, env, t, a) -> (try
				let scope = extend env in (ignore (bind scope x (eval co ce ci scope b)); eval co ce ci scope a)
			with Return a -> a)
		| _ -> raise (StuckTerm "Apply"))

	| TermLambda(x, old, t, a) -> TermLambda(x, env, t, a)

	| TermMap           (f, d) -> (let rec map func data result =
												(match data with
													| TermPair (head,tail) -> let apply = (TermApply (func, head)) in
																										let r = (eval co ce ci env apply) in
																										map func tail (r :: result)
													| TermUnit			-> TermList (List.rev result)
													| _ 						-> raise (StuckTerm "Map")) in
											map (eval co ce ci env f) (eval co ce ci env d) [])
	 | TermFilter (f,d) -> (let rec filter func data result =
		 												match data with
															| TermPair (head,tail) -> let apply = (TermApply (func, head)) in
																												let r = (eval co ce ci env apply) in
																												(match r with
																													| TermNum 0 							 -> filter func tail result
																													| _ 											 -> filter func tail (head :: result))
															| TermUnit -> TermList (List.rev result)
															| _ -> raise (StuckTerm "Filter") in
													filter (eval co ce ci env f) (eval co ce ci env d) [])
	| TermFold (f,d,n) -> (let rec fold func data result =
													(match data with
														| TermPair (head, tail) -> let apply = (TermApply ((TermApply (func, result)), head)) in
																											 let r = (eval co ce ci env apply) in
																											 fold func tail r
														| TermUnit -> (eval co ce ci env result)
														| _ -> raise (StuckTerm "Fold")) in
													fold (eval co ce ci env f) (eval co ce ci env d) (eval co ce ci env n))

	| TermLimit (l,n) -> (let rec limit l n r =
													match l, n with
														| (TermPair(head,tail), TermNum n) when n > 0 -> limit tail (TermNum (n-1)) (head :: r)
														| (_, TermNum 0) | (TermUnit, _) -> List.rev r
														| (_,_) -> raise (StuckTerm "Limit") in
												TermList (limit (eval co ce ci env l) (eval co ce ci env n) []))

	| TermUnit                 -> TermUnit
