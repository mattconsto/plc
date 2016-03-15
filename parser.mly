%{
	open Types
	open Toy
%}
/* Keywords */
%token <int> INT
%token <string> IDENT
%token <string> STRING

%token MINUS PLUS
%token UNARY_NEGATION
%token BINARY_POWER BINARY_MULTIPLY BINARY_DIVIDE BINARY_MODULO
%token BITWISE_LEFT BITWISE_RIGHT
%token COMPARE_LT COMPARE_LTE COMPARE_GT COMPARE_GTE COMPARE_E COMPARE_NE
%token BITWISE_AND BITWISE_XOR BITWISE_OR
%token ASSIGN_EQUAL ASSIGN_ADDITION ASSIGN_SUBTRACT ASSIGN_MULTIPLY ASSIGN_DIVIDE ASSIGN_MODULO ASSIGN_AND ASSIGN_XOR ASSIGN_OR

%token TRUE FALSE
%token LET SET IN IF THEN ELSE FUN MATCH WHILE DONE DO FOR BREAK CONTINUE ASSERT CONS HEAD TAIL PRINT TO_STRING READ

%token LAMBDA ROUNDL ROUNDR ITYPE LTYPE FUNTYPE

/* Brackets */
%token CURLYL CURLYR ROUNDL ROUNDR SQUAREL SQUARER

%token COLON QUESTION
%token EOF SEMI_COLON DOT COMMA

/* Associativity */

/* Low */
%left FUNTYPE
%left LET SET IN IF THEN ELSE FUN MATCH WHILE DONE DO FOR BREAK CONTINUE ASSERT CONS HEAD TAIL PRINT TO_STRING READ
%left IDENT STRING
%left TRUE FALSE
%left ASSIGN_EQUAL ASSIGN_ADDITION ASSIGN_SUBTRACT ASSIGN_MULTIPLY ASSIGN_DIVIDE ASSIGN_MODULO ASSIGN_AND ASSIGN_XOR ASSIGN_OR
%left QUESTION COLON
%left BITWISE_OR
%left BITWISE_XOR
%left BITWISE_AND
%left COMPARE_E COMPARE_NE
%left COMPARE_LT COMPARE_LTE COMPARE_GT COMPARE_GTE
%left BITWISE_LEFT BITWISE_RIGHT
%left BINARY_POWER BINARY_MULTIPLY BINARY_DIVIDE BINARY_MODULO
%left UNARY_NEGATION MINUS PLUS
%left SQUAREL SQUARER
%left ROUNDL ROUNDR
%left CURLYL CURLYR
/* High */

/* Entry Point */
%start parser

/* Return Types */
%type <Types.toyType> type_spec
%type <Types.toyTerm> parser

%%

parser:
	| exprs EOF                                        { TmCons ($1, TmUnit) }
	| EOF                                              { TmUnit }
;

type_spec:
	| ITYPE                                            { ToyInt }
	| LTYPE type_spec type_spec                        { ToyPair ($2, $3) }
	| type_spec FUNTYPE type_spec                      { ToyFun ($1, $3) }
;

exprs:
	| expr exprs                                       { TmCons ($1, $2) }
	| expr                                             { $1 }
;

expr:
	| SEMI_COLON                                       { TmUnit }
	| ROUNDL ROUNDR                                    { TmUnit }

	| loop                                             { $1 }
	| assign                                           { $1 }
	| conditional                                      { $1 }
	| unary                                            { $1 }
	| binary                                           { $1 }
	| compare                                          { $1 }
	| bitwise                                          { $1 }

	| BREAK                                            { TmBreak }
	| CONTINUE                                         { TmContinue }
	| ASSERT expr                                      { TmAssert $2 }
	| READ expr                                        { TmRead $2 }
	| PRINT expr                                       { TmPrint $2 }
	| TO_STRING expr                                   { TmToString $2 }

	| LAMBDA ROUNDL type_spec COLON IDENT ROUNDR expr  { TmAbs ($5, $3, $7) }
	| ROUNDL expr ROUNDR                               { $2 }
	| CURLYL exprs CURLYR                              { $2 }

	| list                                             { $1 }
	| data                                             { $1 }
;

data:
	| FALSE                                            { TmNum 0 }
	| TRUE                                             { TmUnaryNot (TmNum 0) } /* True = !False */
	| INT                                              { TmNum $1 }
	| IDENT                                            { TmVar $1 }
	| STRING                                           { TmString $1 }
;

list:
	| SQUAREL SQUARER                                  { TmUnit }
	| SQUAREL expr DOT expr SQUARER                    { TmCons ($2, $4) }
	| SQUAREL list_inner SQUARER                       { $2 }
	| CONS expr expr                                   { TmCons ($2, $3) }
	| HEAD expr                                        { TmHead $2 }
	| TAIL expr                                        { TmTail $2 }
;

list_inner:
	| expr COMMA list_inner                            { TmCons ($1, $3)}
	| expr COMMA                                       { TmCons ($1, TmUnit) }
	| expr                                             { TmCons ($1, TmUnit) }
;

loop:
	| WHILE expr DO expr                               { TmWhile ($2, $4) }
	| DO expr WHILE expr                               { TmDo ($4, $2) }
	| FOR assign SEMI_COLON expr SEMI_COLON expr THEN expr { TmFor ($2, $4, $6, $8)}
;

assign:
	| type_spec IDENT ASSIGN_EQUAL expr            { TmLet ($2, $1, $4) }
	| type_spec IDENT ASSIGN_ADDITION expr         { TmLet ($2, $1, TmPlus ((TmVar $2), $4)) }
	| type_spec IDENT ASSIGN_SUBTRACT expr         { TmLet ($2, $1, TmSubtract ((TmVar $2), $4)) }
	| type_spec IDENT ASSIGN_MULTIPLY expr         { TmLet ($2, $1, TmMultiply ((TmVar $2), $4)) }
	| type_spec IDENT ASSIGN_DIVIDE expr           { TmLet ($2, $1, TmDivide ((TmVar $2), $4)) }
	| type_spec IDENT ASSIGN_MODULO expr           { TmLet ($2, $1, TmModulo ((TmVar $2), $4)) }
	| type_spec IDENT ASSIGN_AND expr              { TmLet ($2, $1, TmBitwiseAnd ((TmVar $2), $4)) }
	| type_spec IDENT ASSIGN_XOR expr              { TmLet ($2, $1, TmBitwiseXOr ((TmVar $2), $4)) }
	| type_spec IDENT ASSIGN_OR expr               { TmLet ($2, $1, TmBitwiseOr ((TmVar $2), $4)) }

	| IDENT ASSIGN_EQUAL expr                      { TmReBind ($1, $3) }
	| IDENT ASSIGN_ADDITION expr                   { TmReBind ($1, TmPlus ((TmVar $1), $3)) }
	| IDENT ASSIGN_SUBTRACT expr                   { TmReBind ($1, TmSubtract ((TmVar $1), $3)) }
	| IDENT ASSIGN_MULTIPLY expr                   { TmReBind ($1, TmMultiply ((TmVar $1), $3)) }
	| IDENT ASSIGN_DIVIDE expr                     { TmReBind ($1, TmDivide ((TmVar $1), $3)) }
	| IDENT ASSIGN_MODULO expr                     { TmReBind ($1, TmModulo ((TmVar $1), $3)) }
	| IDENT ASSIGN_AND expr                        { TmReBind ($1, TmBitwiseAnd ((TmVar $1), $3)) }
	| IDENT ASSIGN_XOR expr                        { TmReBind ($1, TmBitwiseXOr ((TmVar $1), $3)) }
	| IDENT ASSIGN_OR expr                         { TmReBind ($1, TmBitwiseOr ((TmVar $1), $3)) }
;

conditional:
	| IF expr THEN expr ELSE expr                      { TmIf ($2, $4, $6) }
	| expr QUESTION expr COLON expr                    { TmIf ($1, $3, $5) }
	| expr QUESTION QUESTION expr                      { TmIf ($1, $1, $4) }
;

unary:
	| UNARY_NEGATION expr                              { TmUnaryNot $2 }
	| MINUS expr                                       { TmUnaryMinus $2 }
	| PLUS expr                                        { TmUnaryPlus $2 }
;

binary:
	| expr BINARY_POWER expr                           { TmPower ($1, $3) }
	| expr BINARY_MULTIPLY expr                        { TmMultiply ($1, $3) }
	| expr BINARY_DIVIDE expr                          { TmDivide ($1, $3) }
	| expr BINARY_MODULO expr                          { TmModulo ($1, $3) }
	| expr PLUS expr                                   { TmPlus ($1, $3) }
	| expr MINUS expr                                  { TmSubtract ($1, $3) }
;

compare:
	| expr COMPARE_LT expr                             { TmLessThan ($1, $3) }
	| expr COMPARE_LTE expr                            { TmLessThanEqual($1, $3) }
	| expr COMPARE_GT expr                             { TmMoreThan($1, $3) }
	| expr COMPARE_GTE expr                            { TmMoreThanEqual($1, $3) }
	| expr COMPARE_E expr                              { TmEqual($1, $3) }
	| expr COMPARE_NE expr                             { TmNotEqual($1, $3) }
;

bitwise:
	| expr BITWISE_LEFT expr                           { TmShiftLeft($1, $3) }
	| expr BITWISE_RIGHT expr                          { TmShiftRight($1, $3) }
	| expr BITWISE_AND expr                            { TmBitwiseAnd($1, $3) }
	| expr BITWISE_XOR expr                            { TmBitwiseXOr($1, $3) }
	| expr BITWISE_OR expr                             { TmBitwiseOr($1, $3) }
;
