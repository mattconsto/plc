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
