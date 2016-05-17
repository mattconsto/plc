open Environment
open Printf

type aquaType = TypeUnit | TypeNum | TypePair of aquaType * aquaType | TypeList of aquaType | TypeFun of aquaType * aquaType

(* Grammar of the language *)
type aquaTerm =
	| TermUnit
	| TermNum of int
	| TermPair of aquaTerm * aquaTerm
	| TermList of aquaTerm list
	| TermString of string

	| TermLessThan of aquaTerm * aquaTerm
	| TermLessThanEqual of aquaTerm * aquaTerm
	| TermMoreThan of aquaTerm * aquaTerm
	| TermMoreThanEqual of aquaTerm * aquaTerm
	| TermEqual of aquaTerm * aquaTerm
	| TermNotEqual of aquaTerm * aquaTerm

	| TermUnaryNot of aquaTerm
	| TermUnaryMinus of aquaTerm
	| TermUnaryPlus of aquaTerm

	| TermPower of aquaTerm * aquaTerm
	| TermMultiply of aquaTerm * aquaTerm
	| TermDivide of aquaTerm * aquaTerm
	| TermModulo of aquaTerm * aquaTerm
	| TermPlus of aquaTerm * aquaTerm
	| TermSubtract of aquaTerm * aquaTerm

	| TermStringLower of aquaTerm
	| TermStringUpper of aquaTerm
	| TermStringRev of aquaTerm

	| TermMathMin of aquaTerm * aquaTerm
	| TermMathMax of aquaTerm * aquaTerm
	| TermMathAbs of aquaTerm
	| TermMathSign of aquaTerm
	| TermMathSqrt of aquaTerm
	| TermMathLog of aquaTerm
	| TermMathLn of aquaTerm
	| TermMathFact of aquaTerm

	| TermShiftLeft of aquaTerm * aquaTerm
	| TermShiftRight of aquaTerm * aquaTerm
	| TermBitwiseAnd of aquaTerm * aquaTerm
	| TermBitwiseXOr of aquaTerm * aquaTerm
	| TermBitwiseOr of aquaTerm * aquaTerm

	| TermScope of aquaTerm
	| TermWhile of aquaTerm * aquaTerm
	| TermDo of aquaTerm * aquaTerm
	| TermFor of aquaTerm * aquaTerm * aquaTerm * aquaTerm

	| TermBreak
	| TermContinue
	| TermReturn of aquaTerm
	| TermAssert of aquaTerm
	| TermExit of aquaTerm

	| TermReadInt
	| TermReadString
	| TermReadBool

	| TermClear

	| TermPrintInt of aquaTerm
	| TermPrintString of aquaTerm
	| TermPrintBool of aquaTerm

	| TermErrorInt of aquaTerm
	| TermErrorString of aquaTerm
	| TermErrorBool of aquaTerm

	| TermRandom of aquaTerm * aquaTerm

	| TermCons of aquaTerm * aquaTerm
	| TermConsFirst of aquaTerm * aquaTerm
	| TermConsLast of aquaTerm * aquaTerm
	| TermHead of aquaTerm
	| TermTail of aquaTerm
	| TermLength of aquaTerm

	| TermVar of string
	| TermIf of aquaTerm * aquaTerm * aquaTerm
	| TermBind of string * aquaType * aquaTerm
	| TermReBind of string * aquaTerm
	| TermUnBind of string
	| TermLambda of string * aquaTerm environment * aquaType * aquaTerm
	| TermApply of aquaTerm * aquaTerm

	| TermMap of aquaTerm * aquaTerm
	| TermFold of aquaTerm * aquaTerm * aquaTerm
	| TermFilter of aquaTerm * aquaTerm
	| TermLimit of aquaTerm * aquaTerm

let global_types  = (extend Head:aquaType environment)
let global_values = (extend Head:aquaTerm environment)

let rec type_to_string aquaType = match aquaType with
	| TypeUnit                  -> "unit"
	| TypeNum                   -> "int"
	| TypePair           (a, b) -> sprintf "pair<%s, %s>" (type_to_string a) (type_to_string b)
	| TypeList                a -> sprintf "list<%s>" (type_to_string a)
	| TypeFun            (a, b) -> sprintf "%s → %s" (type_to_string a) (type_to_string b)

let rec term_to_string aquaTerm = match aquaTerm with
	| TermUnit                  -> "[]"
	| TermNum                 n -> sprintf "%d" n
	| TermPair           (a, b) -> let rec builder a b = (match a, b with
			| m, TermPair (n, o) -> sprintf "%s,%s" (term_to_string m) (builder n o)
			| m, TermUnit        -> (term_to_string m)
			| m, n               -> sprintf "%s.%s" (term_to_string m) (term_to_string n)
		) in sprintf "[%s]" (builder a b)
	| TermList                _ -> "TODO LIST"
	| TermString              s -> s

	| TermLessThan       (a, b) -> sprintf "%s < %s" (term_to_string a) (term_to_string b)
	| TermLessThanEqual  (a, b) -> sprintf "%s ≤ %s" (term_to_string a) (term_to_string b)
	| TermMoreThan       (a, b) -> sprintf "%s > %s" (term_to_string a) (term_to_string b)
	| TermMoreThanEqual  (a, b) -> sprintf "%s ≥ %s" (term_to_string a) (term_to_string b)
	| TermEqual          (a, b) -> sprintf "%s = %s" (term_to_string a) (term_to_string b)
	| TermNotEqual       (a, b) -> sprintf "%s ≠ %s" (term_to_string a) (term_to_string b)

	| TermUnaryNot            a -> sprintf "¬%s" (term_to_string a)
	| TermUnaryMinus          a -> sprintf "-%s" (term_to_string a)
	| TermUnaryPlus           a -> sprintf "+%s" (term_to_string a)

	| TermPower          (a, b) -> sprintf "%s ^ %s" (term_to_string a) (term_to_string b)
	| TermMultiply       (a, b) -> sprintf "%s * %s" (term_to_string a) (term_to_string b)
	| TermDivide         (a, b) -> sprintf "%s / %s" (term_to_string a) (term_to_string b)
	| TermModulo         (a, b) -> sprintf "%s %% %s" (term_to_string a) (term_to_string b)
	| TermPlus           (a, b) -> sprintf "%s + %s" (term_to_string a) (term_to_string b)
	| TermSubtract       (a, b) -> sprintf "%s - %s" (term_to_string a) (term_to_string b)

	| TermStringLower         s -> sprintf "strinng.lower %s" (term_to_string s)
	| TermStringUpper         s -> sprintf "strinng.upper %s" (term_to_string s)
	| TermStringRev           s -> sprintf "strinng.rev %s" (term_to_string s)

	| TermMathMin        (a, b) -> sprintf "math.min %s %s" (term_to_string a) (term_to_string b)
	| TermMathMax        (a, b) -> sprintf "math.max %s %s" (term_to_string a) (term_to_string b)
	| TermMathAbs             a -> sprintf "math.abs %s" (term_to_string a)
	| TermMathSign            a -> sprintf "math.sign %s" (term_to_string a)
	| TermMathSqrt            a -> sprintf "math.sqrt %s" (term_to_string a)
	| TermMathLog             a -> sprintf "math.log %s" (term_to_string a)
	| TermMathLn              a -> sprintf "math.ln %s" (term_to_string a)
	| TermMathFact            a -> sprintf "math.fact %s" (term_to_string a)

	| TermShiftLeft      (a, b) -> sprintf "%s << %s" (term_to_string a) (term_to_string b)
	| TermShiftRight     (a, b) -> sprintf "%s >> %s" (term_to_string a) (term_to_string b)
	| TermBitwiseAnd     (a, b) -> sprintf "%s ∧ %s" (term_to_string a) (term_to_string b)
	| TermBitwiseXOr     (a, b) -> sprintf "%s ⊻ %s" (term_to_string a) (term_to_string b)
	| TermBitwiseOr      (a, b) -> sprintf "%s ∨ %s" (term_to_string a) (term_to_string b)

	| TermScope               a -> sprintf "{%s}" (term_to_string a)
	| TermWhile          (p, a) -> sprintf "while %s do %s" (term_to_string p) (term_to_string a)
	| TermDo             (p, a) -> sprintf "do %s while %s" (term_to_string a) (term_to_string p)
	| TermFor      (a, b, c, d) -> sprintf "for %s; %s; %s then %s" (term_to_string a) (term_to_string b) (term_to_string c) (term_to_string d)

	| TermBreak                 -> "break"
	| TermContinue              -> "continue"
	| TermReturn              a -> sprintf "return %s" (term_to_string a)
	| TermAssert              a -> sprintf "assert %s" (term_to_string a)
	| TermExit                a -> sprintf "exit %s" (term_to_string a)

	| TermReadInt               -> "io.readi"
	| TermReadString            -> "io.reads"
	| TermReadBool              -> "io.readb"

	| TermClear                 -> "io.clear"

	| TermPrintInt            a -> sprintf "io.printi %s" (term_to_string a)
	| TermPrintString         a -> sprintf "io.prints %s" (term_to_string a)
	| TermPrintBool           a -> sprintf "io.printb %s" (term_to_string a)

	| TermErrorInt            a -> sprintf "io.errori %s" (term_to_string a)
	| TermErrorString         a -> sprintf "io.errors %s" (term_to_string a)
	| TermErrorBool           a -> sprintf "io.errorb %s" (term_to_string a)

	| TermRandom         (a, b) -> sprintf "math.rand %s %s" (term_to_string a) (term_to_string b)

	| TermCons           (a, b) -> let rec builder a b = (match a, b with
			| m, TermCons (n, o) -> sprintf "%s,%s" (term_to_string m) (builder n o)
			| m, TermUnit        -> (term_to_string m)
			| m, n               -> sprintf "%s.%s" (term_to_string m) (term_to_string n)
		) in sprintf "[%s]" (builder a b)
	| TermConsFirst      (a, b) -> let rec builder a b = (match a, b with
			| m, TermConsFirst (n, o) -> sprintf "%s,%s" (term_to_string m) (builder n o)
			| m, TermUnit             -> (term_to_string m)
			| m, n                    -> sprintf "%s.%s" (term_to_string m) (term_to_string n)
		) in sprintf "|%s]" (builder a b)
	| TermConsLast       (a, b) -> let rec builder a b = (match a, b with
			| m, TermConsLast (n, o) -> sprintf "%s,%s" (term_to_string m) (builder n o)
			| m, TermUnit            -> (term_to_string m)
			| m, n                   -> sprintf "%s.%s" (term_to_string m) (term_to_string n)
		) in sprintf "[%s|" (builder a b)
	| TermHead                a -> sprintf "head %s" (term_to_string a)
	| TermTail                a -> sprintf "tail%s" (term_to_string a)
	| TermLength              a -> sprintf "#%s" (term_to_string a)
	| TermMap            (a, b) -> sprintf "list.map %s %s" (term_to_string a) (term_to_string b)
	| TermFold        (a, b, c) -> sprintf "list.fold %s %s %s" (term_to_string a) (term_to_string b) (term_to_string c)
	| TermFilter         (a, b) -> sprintf "list.filter %s %s" (term_to_string a) (term_to_string b)
	| TermLimit          (a, b) -> sprintf "list.limit %s %s" (term_to_string a) (term_to_string b)

	| TermVar                a -> a
	| TermIf         (p, a, b) -> sprintf "%s ? %s : %s" (term_to_string p) (term_to_string a) (term_to_string b)
	| TermBind       (i, t, e) -> sprintf "%s %s = %s" (type_to_string t) i (term_to_string e)
	| TermReBind        (i, e) -> sprintf "%s = %s" i (term_to_string e)
	| TermUnBind             i -> sprintf "unbind %s" i
	| TermLambda (i, v, TypeUnit, e) -> sprintf "λ() %s" (term_to_string e)
	| TermLambda  (i, v, t, e) -> sprintf "λ(%s %s) %s" (type_to_string t) i (term_to_string e)
	| TermApply         (a, b) -> sprintf "%s(%s)" (term_to_string a) (term_to_string b)
