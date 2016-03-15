(* Types of the language *)
type toyType = ToyUnit | ToyInt | ToyPair of toyType * toyType | ToyFun of toyType * toyType

(* Grammar of the language *)
type toyTerm =
	| TmUnit
	| TmNum of int
	| TmPair of toyTerm * toyTerm
	| TmString of string

	| TmLessThan of toyTerm * toyTerm
	| TmLessThanEqual of toyTerm * toyTerm
	| TmMoreThan of toyTerm * toyTerm
	| TmMoreThanEqual of toyTerm * toyTerm
	| TmEqual of toyTerm * toyTerm
	| TmNotEqual of toyTerm * toyTerm

	| TmUnaryNot of toyTerm
	| TmUnaryMinus of toyTerm
	| TmUnaryPlus of toyTerm

	| TmPower of toyTerm * toyTerm
	| TmMultiply of toyTerm * toyTerm
	| TmDivide of toyTerm * toyTerm
	| TmModulo of toyTerm * toyTerm
	| TmPlus of toyTerm * toyTerm
	| TmSubtract of toyTerm * toyTerm

	| TmShiftLeft of toyTerm * toyTerm
	| TmShiftRight of toyTerm * toyTerm
	| TmBitwiseAnd of toyTerm * toyTerm
	| TmBitwiseXOr of toyTerm * toyTerm
	| TmBitwiseOr of toyTerm * toyTerm

	| TmWhile of toyTerm * toyTerm
	| TmDo of toyTerm * toyTerm
	| TmFor of toyTerm * toyTerm * toyTerm * toyTerm
	| TmBreak
	| TmContinue
	| TmAssert of toyTerm
	| TmRead of toyTerm
	| TmPrint of toyTerm
	| TmToString of toyTerm

	| TmCons of toyTerm * toyTerm
	| TmHead of toyTerm
	| TmTail of toyTerm

	| TmVar of string
	| TmIf of toyTerm * toyTerm * toyTerm
	| TmLet of string * toyType * toyTerm
	| TmReBind of string * toyTerm
	| TmAbs of string * toyType * toyTerm
