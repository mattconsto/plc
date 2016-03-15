{
	open Parser
	open String
	open Str

	exception SyntaxError of string
}
rule lexer = parse
	(* whitespace *)
	| [' ''\t''\n']                       { lexer lexbuf }

	| '/'[' ''\t''\n']*'*'                { ignore (read_comment (Buffer.create 1000) lexbuf); lexer lexbuf }

	(* Identifier *)
	| "⊤" | "true"  | "True"              { TRUE }
	| "⊥" | "false" | "False"             { FALSE }

	| "int"  | "Int"                      { ITYPE }
	| "list" | "List"                     { LTYPE }
	| "->"   | "→"                        { FUNTYPE }
	| "\\l"  | "lambda" | "Lambda"        { LAMBDA }

	| "let"       | "Let"                 { LET }
	| "set"       | "Set"                 { SET }
	| "in"        | "In"                  { IN }
	| "if"        | "If"                  { IF }
	| "then"      | "Then"                { THEN }
	| "else"      | "Else"                { ELSE }
	| "fun"       | "Fun"                 { FUN }
	| "match"     | "Match"               { MATCH }
	| "while"     | "While"               { WHILE }
	| "done"      | "Done"                { DONE }
	| "do"        | "Do"                  { DO }
	| "for"       | "For"                 { FOR }
	| "break"     | "Break"               { BREAK }
	| "continue"  | "Continue"            { CONTINUE }
	| "assert"    | "Assert"              { ASSERT }
	| "cons"      | "Cons"                { CONS }
	| "head"      | "Head"                { HEAD }
	| "tail"      | "Tail"                { TAIL }
	| "string"    | "String"              { TO_STRING }
	| "print"     | "Print"               { PRINT }
	| "read"      | "Read"                { READ }

	(* Brackets *)
	| '{'                                 { CURLYL }
	| '}'                                 { CURLYR }
	| '('                                 { ROUNDL }
	| ')'                                 { ROUNDR }
	| '['                                 { SQUAREL }
	| ']'                                 { SQUARER }

	| ':'                                 { COLON }
	| '?'                                 { QUESTION }


	| '-'                                 { MINUS }
	| '+'                                 { PLUS }

	| '!' | "¬" | '~' | "not" | "Not"     { UNARY_NEGATION }

	| '^' | "pow" | "Pow"                 { BINARY_POWER }
	| '*'                                 { BINARY_MULTIPLY }
	| '/'                                 { BINARY_DIVIDE }
	| '%' | "mod" | "Mod"                 { BINARY_MODULO }

	| "<<"                                { BITWISE_LEFT }
	| ">>"                                { BITWISE_RIGHT }

	| '<'                                 { COMPARE_LT }
	| "≤" | "<="                          { COMPARE_LTE }
	| '>'                                 { COMPARE_GT }
	| "≥" | ">="                          { COMPARE_GTE }
	| "=="                                { COMPARE_E }
	| "≠" | "!="                          { COMPARE_NE }

	| '&' | "∧" | "and" | "And"           { BITWISE_AND }
	| '^' | "⊻" | "xor" | "XOr"           { BITWISE_XOR }
	| '|' | "∨" | "⊕"  | "or" | "Or"      { BITWISE_OR }

	| "+="                                { ASSIGN_ADDITION }
	| "-="                                { ASSIGN_SUBTRACT }
	| "*="                                { ASSIGN_MULTIPLY }
	| "/="                                { ASSIGN_DIVIDE }
	| "%="                                { ASSIGN_MODULO }
	| "&="                                { ASSIGN_AND }
	| "^="                                { ASSIGN_XOR }
	| "|="                                { ASSIGN_OR }
	| '='                                 { ASSIGN_EQUAL }

	(* Need to be after keywords *)
	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''-''_']* as v { IDENT v }
	| '-'?           ['0'-'9''_']+ as v
	| '-'?'0'['b''B']['0'-'1''_']+ as v
	| '-'?'0'['o''O']['0'-'7''_']+ as v
	| '-'?'0'['x''X']['0'-'9''a'-'f''_']+ as v { INT (int_of_string (global_replace (regexp "_") "" v)) }
	| '\'' [^ '\''] '\'' as v             { INT (Char.code v.[1])}
	| '"'                                 { STRING ( read_string (Buffer.create 100) lexbuf ) }

	| '.'                                 { DOT }
	| ','                                 { COMMA }
	| ';'                                 { SEMI_COLON }
	| eof                                 { EOF }

and read_string buf = parse
	| '"'                                 { Buffer.contents buf }
	| '\\' '/'                            { Buffer.add_char buf '/'; read_string buf lexbuf }
	| '\\' '\\'                           { Buffer.add_char buf '\\'; read_string buf lexbuf }
	| '\\' 'b'                            { Buffer.add_char buf '\b'; read_string buf lexbuf }
	| '\\' 'f'                            { Buffer.add_char buf '\012'; read_string buf lexbuf }
	| '\\' 'n'                            { Buffer.add_char buf '\n'; read_string buf lexbuf }
	| '\\' 'r'                            { Buffer.add_char buf '\r'; read_string buf lexbuf }
	| '\\' 't'                            { Buffer.add_char buf '\t'; read_string buf lexbuf }
	| [^ '"' '\\']+                       { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
	| _                                   { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
	| eof                                 { raise (SyntaxError ("String is not terminated!")) }

and read_comment buf = parse
	| '*'[' ''\t''\n']*'/'                { Buffer.contents buf }
	| eof                                 { raise (SyntaxError ("Comment is not terminated!")) }
	| _                                   { Buffer.add_string buf (Lexing.lexeme lexbuf); read_comment buf lexbuf}
