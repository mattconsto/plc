open Printf

open Types
open Environment

type term_to_stringf = DefaultFormatting | IntegerFormatting | StringFormatting | BooleanFormatting

let rec type_to_string t = match t with
	| TypeUnit                  -> "unit"
	| TypeNum                   -> "int"
	| TypePair           (a, b) -> sprintf "pair<%s, %s>" (type_to_string a) (type_to_string b)
	| TypeList                a -> sprintf "list<%s>" (type_to_string a)
	| TypeFun            (a, b) -> sprintf "%s → %s" (type_to_string a) (type_to_string b)

let rec term_to_stringf t f = match t with
	| TermUnit                  -> (match f with
			| StringFormatting  -> ""
			| _                 -> "[]"
		)
	| TermNum                 n -> (match f with
			| StringFormatting  -> String.make 1 (Char.chr n)
			| BooleanFormatting -> if n != 0 then "true" else "false"
			| _                 -> sprintf "%d" n
		)
	| TermPair           (a, b) -> let rec builder a b = (match a, b with
			| m, TermPair (n, o) -> (match f with
					| StringFormatting  -> sprintf "%s%s" (term_to_stringf m f) (builder n o)
					| BooleanFormatting
					| IntegerFormatting -> sprintf "%s %s" (term_to_stringf m f) (builder n o)
					| _                 -> sprintf "%s,%s" (term_to_stringf m f) (builder n o)
				)
			| m, TermUnit        -> (term_to_stringf m f)
			| m, n               -> (match f with
					| StringFormatting  -> sprintf "%s%s" (term_to_stringf m f) (term_to_stringf n f)
					| BooleanFormatting
					| IntegerFormatting -> sprintf "%s %s" (term_to_stringf m f) (term_to_stringf n f)
					| _                 -> sprintf "%s.%s" (term_to_stringf m f) (term_to_stringf n f)
				)
		) in (match f with
				| StringFormatting
				| BooleanFormatting
				| IntegerFormatting -> sprintf "%s" (builder a b)
				| _                 -> sprintf "[%s]" (builder a b)
			)
	| TermList                _ -> "TODO LIST"
	| TermString              s -> sprintf "\"%s\"" s

	| TermLessThan       (a, b) -> sprintf "%s < %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermLessThanEqual  (a, b) -> sprintf "%s ≤ %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermMoreThan       (a, b) -> sprintf "%s > %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermMoreThanEqual  (a, b) -> sprintf "%s ≥ %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermEqual          (a, b) -> sprintf "%s = %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermNotEqual       (a, b) -> sprintf "%s ≠ %s" (term_to_stringf a f) (term_to_stringf b f)

	| TermUnaryNot            a -> sprintf "¬%s" (term_to_stringf a f)
	| TermUnaryMinus          a -> sprintf "-%s" (term_to_stringf a f)
	| TermUnaryPlus           a -> sprintf "+%s" (term_to_stringf a f)

	| TermPower          (a, b) -> sprintf "%s ^ %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermMultiply       (a, b) -> sprintf "%s * %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermDivide         (a, b) -> sprintf "%s / %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermModulo         (a, b) -> sprintf "%s %% %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermPlus           (a, b) -> sprintf "%s + %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermSubtract       (a, b) -> sprintf "%s - %s" (term_to_stringf a f) (term_to_stringf b f)

	| TermStringLower         s -> sprintf "string.lower %s" (term_to_stringf s f)
	| TermStringUpper         s -> sprintf "string.upper %s" (term_to_stringf s f)
	| TermStringRev           s -> sprintf "string.rev %s" (term_to_stringf s f)

	| TermMathMin        (a, b) -> sprintf "math.min %s %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermMathMax        (a, b) -> sprintf "math.max %s %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermMathAbs             a -> sprintf "math.abs %s" (term_to_stringf a f)
	| TermMathSign            a -> sprintf "math.sign %s" (term_to_stringf a f)
	| TermMathSqrt            a -> sprintf "math.sqrt %s" (term_to_stringf a f)
	| TermMathLog             a -> sprintf "math.log %s" (term_to_stringf a f)
	| TermMathLn              a -> sprintf "math.ln %s" (term_to_stringf a f)
	| TermMathFact            a -> sprintf "math.fact %s" (term_to_stringf a f)

	| TermShiftLeft      (a, b) -> sprintf "%s << %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermShiftRight     (a, b) -> sprintf "%s >> %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermBitwiseAnd     (a, b) -> sprintf "%s ∧ %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermBitwiseXOr     (a, b) -> sprintf "%s ⊻ %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermBitwiseOr      (a, b) -> sprintf "%s ∨ %s" (term_to_stringf a f) (term_to_stringf b f)

	| TermScope               a -> sprintf "{%s}" (term_to_stringf a f)
	| TermWhile          (p, a) -> sprintf "while %s do %s" (term_to_stringf p f) (term_to_stringf a f)
	| TermDo             (p, a) -> sprintf "do %s while %s" (term_to_stringf a f) (term_to_stringf p f)
	| TermFor      (a, b, c, d) -> sprintf "for %s; %s; %s then %s" (term_to_stringf a f) (term_to_stringf b f) (term_to_stringf c f) (term_to_stringf d f)

	| TermBreak                 -> "break"
	| TermContinue              -> "continue"
	| TermReturn              a -> sprintf "return %s" (term_to_stringf a f)
	| TermAssert              a -> sprintf "assert %s" (term_to_stringf a f)
	| TermExit                a -> sprintf "exit %s" (term_to_stringf a f)

	| TermReadInt               -> "io.readi"
	| TermReadString            -> "io.reads"
	| TermReadBool              -> "io.readb"

	| TermClear                 -> "io.clear"

	| TermPrintInt            a -> sprintf "io.printi %s" (term_to_stringf a f)
	| TermPrintString         a -> sprintf "io.prints %s" (term_to_stringf a f)
	| TermPrintBool           a -> sprintf "io.printb %s" (term_to_stringf a f)

	| TermErrorInt            a -> sprintf "io.errori %s" (term_to_stringf a f)
	| TermErrorString         a -> sprintf "io.errors %s" (term_to_stringf a f)
	| TermErrorBool           a -> sprintf "io.errorb %s" (term_to_stringf a f)

	| TermRandom         (a, b) -> sprintf "math.rand %s %s" (term_to_stringf a f) (term_to_stringf b f)

	| TermCons           (a, b) -> let rec builder a b = (match a, b with
			| m, TermCons (n, o) -> sprintf "%s,%s" (term_to_stringf m f) (builder n o)
			| m, TermUnit        -> (term_to_stringf m f)
			| m, n               -> sprintf "%s.%s" (term_to_stringf m f) (term_to_stringf n f)
		) in sprintf "[%s]" (builder a b)
	| TermConsFirst (TermPrintInt a, TermPrintString (TermNum 10)) -> sprintf "io.printlni %s" (term_to_stringf a f)
	| TermConsFirst (TermPrintString a, TermPrintString (TermNum 10)) -> sprintf "io.printlns %s" (term_to_stringf a f)
	| TermConsFirst (TermPrintBool a, TermPrintString (TermNum 10)) -> sprintf "io.printlnb %s" (term_to_stringf a f)
	| TermConsFirst (TermErrorInt a, TermPrintString (TermNum 10)) -> sprintf "io.errorlni %s" (term_to_stringf a f)
	| TermConsFirst (TermErrorString a, TermPrintString (TermNum 10)) -> sprintf "io.errorlns %s" (term_to_stringf a f)
	| TermConsFirst (TermErrorBool a, TermPrintString (TermNum 10)) -> sprintf "io.errorlnb %s" (term_to_stringf a f)
	| TermConsFirst      (a, b) -> let rec builder a b = (match a, b with
			| m, TermConsFirst (n, o) -> sprintf "%s,%s" (term_to_stringf m f) (builder n o)
			| m, TermUnit             -> (term_to_stringf m f)
			| m, n                    -> sprintf "%s.%s" (term_to_stringf m f) (term_to_stringf n f)
		) in sprintf "|%s]" (builder a b)
	| TermConsLast       (a, b) -> let rec builder a b = (match a, b with
			| m, TermConsLast (n, o) -> sprintf "%s,%s" (term_to_stringf m f) (builder n o)
			| m, TermUnit            -> (term_to_stringf m f)
			| m, n                   -> sprintf "%s.%s" (term_to_stringf m f) (term_to_stringf n f)
		) in sprintf "[%s|" (builder a b)
	| TermHead                a -> sprintf "head %s" (term_to_stringf a f)
	| TermTail                a -> sprintf "tail%s" (term_to_stringf a f)
	| TermLength              a -> sprintf "#%s" (term_to_stringf a f)
	| TermMap            (a, b) -> sprintf "list.map %s %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermFold        (a, b, c) -> sprintf "list.fold %s %s %s" (term_to_stringf a f) (term_to_stringf b f) (term_to_stringf c f)
	| TermFilter         (a, b) -> sprintf "list.filter %s %s" (term_to_stringf a f) (term_to_stringf b f)
	| TermLimit          (a, b) -> sprintf "list.limit %s %s" (term_to_stringf a f) (term_to_stringf b f)

	| TermVar                 a -> a
	| TermIf          (p, a, b) -> sprintf "%s ? %s : %s" (term_to_stringf p f) (term_to_stringf a f) (term_to_stringf b f)
	| TermAutoBind       (i, e) -> sprintf "let %s = %s" i (term_to_stringf e f)
	| TermBind        (i, t, e) -> sprintf "%s %s = %s" (type_to_string t) i (term_to_stringf e f)
	| TermReBind         (i, e) -> sprintf "%s = %s" i (term_to_stringf e f)
	| TermUnBind              i -> sprintf "unbind %s" i
	| TermLambda (i, v, TypeUnit, e) -> sprintf "λ() %s" (term_to_stringf e f)
	| TermLambda   (i, v, t, e) -> sprintf "λ(%s %s) %s" (type_to_string t) i (term_to_stringf e f)
	| TermApply          (a, b) -> sprintf "%s(%s)" (term_to_stringf a f) (term_to_stringf b f)

let term_to_string t = term_to_stringf t DefaultFormatting

let rec environment_to_string env = let rec entries_to_string l = match l with
	| [] -> ""
	| (h, a) :: t -> h ^ "," ^ (entries_to_string t)
in match env with
	| Head -> "Head"
	| Env (parent, entries) -> "[" ^ (environment_to_string parent) ^ ":" ^ (entries_to_string !entries) ^ "]"
