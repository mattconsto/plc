%{
	open Types
	open Language
%}
/* Keywords */
%token <int> INT
%token <string> IDENT
%token <string> STRING

%token MATH_MIN MATH_MAX MATH_ABS MATH_SIGN MATH_SQRT MATH_LOG MATH_LN MATH_FACT
%token MINUS PLUS
%token UNARY_NEGATION
%token BINARY_POWER BINARY_MULTIPLY BINARY_DIVIDE BINARY_MODULO
%token BITWISE_LEFT BITWISE_RIGHT
%token COMPARE_LT COMPARE_LTE COMPARE_GT COMPARE_GTE COMPARE_E COMPARE_NE
%token BITWISE_AND BITWISE_XOR BITWISE_OR
%token ASSIGN_EQUAL ASSIGN_ADDITION ASSIGN_SUBTRACT ASSIGN_MULTIPLY ASSIGN_DIVIDE ASSIGN_MODULO ASSIGN_AND ASSIGN_XOR ASSIGN_OR

%token TRUE FALSE
%token UNBIND IN IF THEN ELSE FUN MATCH WHILE DONE DO LOOP DONE FOR BREAK CONTINUE RETURN ASSERT EXIT CONS HEAD TAIL CLEAR PRINT_INT PRINT_STRING PRINT_BOOL PRINTLN_INT PRINTLN_STRING PRINTLN_BOOL ERROR_INT ERROR_STRING ERROR_BOOL ERRORLN_INT ERRORLN_STRING ERRORLN_BOOL READ_INT READ_STRING READ_BOOL RANDOM LENGTH

%token LAMBDA ROUNDL ROUNDR UTYPE ITYPE LTYPE PTYPE FUNTYPE
%token MAP FOLD FILTER LIMIT

/* Brackets */
%token CURLYL CURLYR ROUNDL ROUNDR SQUAREL SQUARER

%token COLON QUESTION
%token EOF SEMI_COLON DOT COMMA

/* Associativity */

/* Low */
%right FUNTYPE
%left UNBIND IN IF THEN ELSE FUN MATCH WHILE DONE DO LOOP FOR DONE BREAK CONTINUE RETURN ASSERT EXIT CONS HEAD TAIL CLEAR PRINT_INT PRINT_STRING PRINT_BOOL PRINTLN_INT PRINTLN_STRING PRINTLN_BOOL ERROR_INT ERROR_STRING ERROR_BOOL ERRORLN_INT ERRORLN_STRING ERRORLN_BOOL READ_INT READ_STRING READ_BOOL RANDOM
%left IDENT STRING
%left MATH_MIN MATH_MAX MATH_ABS MATH_SIGN MATH_SQRT MATH_LOG MATH_LN MATH_FACT
%left TRUE FALSE
%left ASSIGN_EQUAL ASSIGN_ADDITION ASSIGN_SUBTRACT ASSIGN_MULTIPLY ASSIGN_DIVIDE ASSIGN_MODULO ASSIGN_AND ASSIGN_XOR ASSIGN_OR
%left QUESTION COLON
%left BITWISE_OR
%left BITWISE_XOR
%left BITWISE_AND
%left COMPARE_E COMPARE_NE
%left COMPARE_LT COMPARE_LTE COMPARE_GT COMPARE_GTE
%left BITWISE_LEFT BITWISE_RIGHT
%left MINUS PLUS
%left BINARY_POWER BINARY_MULTIPLY BINARY_DIVIDE BINARY_MODULO
%left UNARY_NEGATION
%left SQUAREL SQUARER
%left ROUNDL ROUNDR
%left CURLYL CURLYR
/* High */

/* Entry Point */
%start parser

/* Return Types */
%type <Types.aquaType> type_spec
%type <Types.aquaTerm> parser

%%

parser:
	| exprs EOF                                        { $1 }
	| EOF                                              { TermUnit }

type_spec:
	| UTYPE                                            { TypeUnit }
	| ITYPE                                            { TypeNum }
	| PTYPE COMPARE_LT type_spec COMMA type_spec COMPARE_GT { TypePair ($3, $5) }
	| LTYPE COMPARE_LT type_spec COMPARE_GT            { TypeList $3 }
	| type_spec FUNTYPE type_spec                      { TypeFun ($1, $3) }
	| ROUNDL type_spec ROUNDR                          { $2 }

exprs:
	| expr SEMI_COLON exprs                            { TermConsLast ($1, $3) }
	| expr SEMI_COLON                                  { $1 }
	| expr                                             { $1 }

expr:
	| SEMI_COLON                                       { TermUnit }

	| loop                                             { $1 }
	| assign                                           { $1 }
	| conditional                                      { $1 }
	| unary                                            { $1 }
	| binary                                           { $1 }
	| compare                                          { $1 }
	| bitwise                                          { $1 }
	| io                                               { $1 }

	| BREAK                                            { TermBreak }
	| CONTINUE                                         { TermContinue }
	| RETURN expr                                      { TermReturn $2 }
	| RETURN                                           { TermReturn TermUnit }
	| ASSERT expr                                      { TermAssert $2 }
	| EXIT expr                                        { TermExit $2 }
	| EXIT                                             { TermExit (TermNum 0) }

	| UNBIND IDENT                                     { TermUnBind $2 }
	| LAMBDA ROUNDL type_spec IDENT ROUNDR expr        { TermLambda ($4, global_values, $3, $6) }
	| expr ROUNDL expr ROUNDR                          { TermApply ($1, $3) }
	| ROUNDL expr ROUNDR                               { $2 }
	| CURLYL exprs CURLYR                              { TermScope $2 }

	| MAP expr expr																		 { TermMap ($2, $3) }
	| FILTER expr expr																 { TermFilter ($2, $3) }
	| FOLD expr expr																	 { TermFold ($2, $3) }
	| LIMIT expr expr																 	 { TermLimit ($2, $3) }

	| list                                             { $1 }
	| data                                             { $1 }

data:
	| FALSE                                            { TermNum 0 }
	| TRUE                                             { TermUnaryNot (TermNum 0) } /* True = !False */
	| INT                                              { TermNum $1 }
	| IDENT                                            { TermVar $1 }
	| STRING                                           { TermString $1 }

list:
	| SQUAREL SQUARER                                  { TermUnit }
	| SQUAREL expr DOT expr SQUARER                    { TermCons ($2, $4) }
	| SQUAREL list_inner SQUARER                       { $2 }
	| CONS expr expr                                   { TermCons ($2, $3) }
	| HEAD expr                                        { TermHead $2 }
	| TAIL expr                                        { TermTail $2 }
	| LENGTH expr                                      { TermLength $2 }

list_inner:
	| expr COMMA list_inner                            { TermCons ($1, $3)}
	| expr COMMA                                       { TermCons ($1, TermUnit) }
	| expr                                             { TermCons ($1, TermUnit) }

loop:
	| WHILE expr DO expr                               { TermWhile ($2, $4) }
	| LOOP expr                                        { TermWhile (TermUnaryNot (TermNum 0), $2) }
	| DO expr WHILE expr                               { TermDo ($4, $2) }
	| FOR assign SEMI_COLON expr SEMI_COLON expr THEN expr  { TermFor ($2, $4, $6, $8)}

assign:
	| type_spec IDENT ASSIGN_EQUAL expr            { TermBind ($2, $1, $4) }

	| IDENT ASSIGN_EQUAL expr                      { TermReBind ($1, $3) }
	| IDENT ASSIGN_ADDITION expr                   { TermReBind ($1, TermPlus ((TermVar $1), $3)) }
	| IDENT ASSIGN_SUBTRACT expr                   { TermReBind ($1, TermSubtract ((TermVar $1), $3)) }
	| IDENT ASSIGN_MULTIPLY expr                   { TermReBind ($1, TermMultiply ((TermVar $1), $3)) }
	| IDENT ASSIGN_DIVIDE expr                     { TermReBind ($1, TermDivide ((TermVar $1), $3)) }
	| IDENT ASSIGN_MODULO expr                     { TermReBind ($1, TermModulo ((TermVar $1), $3)) }
	| IDENT ASSIGN_AND expr                        { TermReBind ($1, TermBitwiseAnd ((TermVar $1), $3)) }
	| IDENT ASSIGN_XOR expr                        { TermReBind ($1, TermBitwiseXOr ((TermVar $1), $3)) }
	| IDENT ASSIGN_OR expr                         { TermReBind ($1, TermBitwiseOr ((TermVar $1), $3)) }

conditional:
	| IF expr THEN expr ELSE expr DONE                 { TermIf ($2, $4, $6) }
	| IF expr THEN expr DONE                           { TermIf ($2, $4, TermUnit) }
	| expr QUESTION expr COLON expr                    { TermIf ($1, $3, $5) }
	| expr QUESTION QUESTION expr                      { TermIf ($1, $1, $4) }

unary:
	| UNARY_NEGATION expr                              { TermUnaryNot $2 }
	| MINUS expr                                       { TermUnaryMinus $2 }
	| PLUS expr                                        { TermUnaryPlus $2 }

	| MATH_ABS expr                                    { TermMathAbs $2 }
	| MATH_SIGN expr                                   { TermMathSign $2 }
	| MATH_SQRT expr                                   { TermMathSqrt $2 }
	| MATH_LOG expr                                    { TermMathLog $2 }
	| MATH_LN expr                                     { TermMathLn $2 }
	| MATH_FACT expr                                   { TermMathFact $2 }

binary:
	| MATH_MIN expr expr                               { TermMathMin ($2, $3) }
	| MATH_MAX expr expr                               { TermMathMax ($2, $3) }

	| expr BINARY_POWER expr                           { TermPower ($1, $3) }
	| expr BINARY_MULTIPLY expr                        { TermMultiply ($1, $3) }
	| expr BINARY_DIVIDE expr                          { TermDivide ($1, $3) }
	| expr BINARY_MODULO expr                          { TermModulo ($1, $3) }
	| expr PLUS expr                                   { TermPlus ($1, $3) }
	| expr MINUS expr                                  { TermSubtract ($1, $3) }

compare:
	| expr COMPARE_LT expr                             { TermLessThan ($1, $3) }
	| expr COMPARE_LTE expr                            { TermLessThanEqual($1, $3) }
	| expr COMPARE_GT expr                             { TermMoreThan($1, $3) }
	| expr COMPARE_GTE expr                            { TermMoreThanEqual($1, $3) }
	| expr COMPARE_E expr                              { TermEqual($1, $3) }
	| expr COMPARE_NE expr                             { TermNotEqual($1, $3) }

bitwise:
	| expr BITWISE_LEFT expr                           { TermShiftLeft($1, $3) }
	| expr BITWISE_RIGHT expr                          { TermShiftRight($1, $3) }
	| expr BITWISE_AND expr                            { TermBitwiseAnd($1, $3) }
	| expr BITWISE_XOR expr                            { TermBitwiseXOr($1, $3) }
	| expr BITWISE_OR expr                             { TermBitwiseOr($1, $3) }

io:
	| READ_INT                                         { TermReadInt }
	| READ_STRING                                      { TermReadString }
	| READ_BOOL                                        { TermReadBool }

	| CLEAR                                            { TermClear }

	| PRINT_INT expr                                   { TermPrintInt $2 }
	| PRINT_STRING expr                                { TermPrintString $2 }
	| PRINT_BOOL expr                                  { TermPrintBool $2 }

	| PRINTLN_INT expr                                 { TermCons(TermPrintInt $2, TermPrintString (TermNum 10)) }
	| PRINTLN_STRING expr                              { TermCons(TermPrintString $2, TermPrintString (TermNum 10)) }
	| PRINTLN_BOOL expr                                { TermCons(TermPrintBool $2, TermPrintString (TermNum 10)) }

	| ERROR_INT expr                                   { TermErrorInt $2 }
	| ERROR_STRING expr                                { TermErrorString $2 }
	| ERROR_BOOL expr                                  { TermErrorBool $2 }

	| ERRORLN_INT expr                                 { TermCons(TermErrorInt $2, TermErrorString (TermNum 10)) }
	| ERRORLN_STRING expr                              { TermCons(TermErrorString $2, TermErrorString (TermNum 10)) }
	| ERRORLN_BOOL expr                                { TermCons(TermErrorBool $2, TermErrorString (TermNum 10)) }

	| RANDOM expr expr                                 { TermRandom ($2, $3) }
	| RANDOM expr                                      { TermRandom (TermNum 0, $2) }
	| RANDOM                                           { TermRandom (TermNum 0, TermNum 1) }
