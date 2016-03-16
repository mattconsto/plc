type aquaType = TypeUnit | TypeInt | TypePair of aquaType * aquaType | TypeFun of aquaType * aquaType

(* Grammar of the language *)
type aquaTerm =
	| TermUnit
	| TermNum of int
	| TermPair of aquaTerm * aquaTerm
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

	| TermShiftLeft of aquaTerm * aquaTerm
	| TermShiftRight of aquaTerm * aquaTerm
	| TermBitwiseAnd of aquaTerm * aquaTerm
	| TermBitwiseXOr of aquaTerm * aquaTerm
	| TermBitwiseOr of aquaTerm * aquaTerm

	| TermWhile of aquaTerm * aquaTerm
	| TermDo of aquaTerm * aquaTerm
	| TermFor of aquaTerm * aquaTerm * aquaTerm * aquaTerm

	| TermBreak
	| TermContinue
	| TermAssert of aquaTerm
	| TermExit of aquaTerm

	| TermReadInt
	| TermReadString
	| TermReadBool

	| TermPrintInt of aquaTerm
	| TermPrintString of aquaTerm
	| TermPrintBool of aquaTerm

	| TermRandom of aquaTerm * aquaTerm

	| TermCons of aquaTerm * aquaTerm
	| TermHead of aquaTerm
	| TermTail of aquaTerm

	| TermVar of string
	| TermIf of aquaTerm * aquaTerm * aquaTerm
	| TermBind of string * aquaType * aquaTerm
	| TermReBind of string * aquaTerm
	| TermLambda of string * aquaType * aquaTerm
	| TermApply of aquaTerm * aquaTerm
