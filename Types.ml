open Environment

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
	| TypeUnit        -> "Unit"
	| TypeNum         -> "Number"
	| TypePair (a, b) -> Printf.sprintf "Pair<%s, %s>" (type_to_string a) (type_to_string b)
	| TypeList a      -> Printf.sprintf "List<%s>" (type_to_string a)
	| TypeFun (a, b)  -> Printf.sprintf "Lambda<%s, %s>" (type_to_string a) (type_to_string b)

let term_to_string aquaTerm = match aquaTerm with
	| TermUnit -> "TermUnit"
	| TermNum _ -> "TermNum"
	| TermPair _ -> "TermPair"
	| TermList _ -> "TermList"
	| TermString _ -> "TermString"

	| TermLessThan _ -> "TermLessThan"
	| TermLessThanEqual _ -> "TermLessThanEqual"
	| TermMoreThan _ -> "TermMoreThan"
	| TermMoreThanEqual _ -> "TermMoreThanEqual"
	| TermEqual _ -> "TermEqual"
	| TermNotEqual _ -> "TermNotEqual"

	| TermUnaryNot _ -> "TermUnaryNot"
	| TermUnaryMinus _ -> "TermUnaryMinus"
	| TermUnaryPlus _ -> "TermUnaryPlus"

	| TermPower _ -> "TermPower"
	| TermMultiply _ -> "TermMultiply"
	| TermDivide _ -> "TermDivide"
	| TermModulo _ -> "TermModulo"
	| TermPlus _ -> "TermPlus"
	| TermSubtract _ -> "TermSubtract"

	| TermMathMin _ -> "TermMathMin"
	| TermMathMax _ -> "TermMathMax"
	| TermMathAbs _ -> "TermMathAbs"
	| TermMathSign _ -> "TermMathSign"
	| TermMathSqrt _ -> "TermMathSqrt"
	| TermMathLog _ -> "TermMathLog"
	| TermMathLn _ -> "TermMathLn"
	| TermMathFact _ -> "TermMathFact"

	| TermShiftLeft _ -> "TermShiftLeft"
	| TermShiftRight _ -> "TermShiftRight"
	| TermBitwiseAnd _ -> "TermBitwiseAnd"
	| TermBitwiseXOr _ -> "TermBitwiseXOr"
	| TermBitwiseOr _ -> "TermBitwiseOr"

	| TermScope _ -> "TermScope"
	| TermWhile _ -> "TermWhile"
	| TermDo _ -> "TermDo"
	| TermFor _ -> "TermFor"

	| TermBreak -> "TermBreak"
	| TermContinue -> "TermContinue"
	| TermReturn _ -> "TermReturn"
	| TermAssert _ -> "TermAssert"
	| TermExit _ -> "TermExit"

	| TermReadInt -> "TermReadInt"
	| TermReadString -> "TermReadString"
	| TermReadBool -> "TermReadBool"

	| TermClear -> "TermClear"

	| TermPrintInt _ -> "TermPrintInt"
	| TermPrintString _ -> "TermPrintString"
	| TermPrintBool _ -> "TermPrintBool"

	| TermErrorInt _ -> "TermErrorInt"
	| TermErrorString _ -> "TermErrorString"
	| TermErrorBool _ -> "TermErrorBool"

	| TermRandom _ -> "TermRandom"

	| TermCons _ -> "TermCons"
	| TermConsFirst _ -> "TermConsFirst"
	| TermConsLast _ -> "TermConsLast"
	| TermHead _ -> "TermHead"
	| TermTail _ -> "TermTail"
	| TermLength _ -> "TermLength"

	| TermVar _ -> "TermVar"
	| TermIf _ -> "TermIf"
	| TermBind _ -> "TermBind"
	| TermReBind _ -> "TermReBind"
	| TermUnBind _ -> "TermRUnBind"
	| TermLambda _ -> "TermLambda"
	| TermApply _ -> "TermApply"
