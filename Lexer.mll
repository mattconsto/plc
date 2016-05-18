{
	open Parser
	open Lexing

	exception SyntaxError of string

	let error msg start finish = Printf.sprintf "(line %d: char %d..%d): %s" start.Lexing.pos_lnum (start.pos_cnum -start.pos_bol) (finish.pos_cnum - finish.pos_bol) msg
	let lex_error lexbuf = raise ( SyntaxError (error (lexeme lexbuf) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)))
}
rule lexer = parse
	(* whitespace *)
	| "//"                                                                        { ignore (read_line_comment (Buffer.create 1000) lexbuf); lexer lexbuf }
	| '/'[' ''\t''\n''\r']*'*'                                                    { ignore (read_block_comment (Buffer.create 1000) lexbuf); lexer lexbuf }

	| [' ''\t''\n''\r']                                                           { lexer lexbuf }

	(* Identifier *)
	| "⊤" | "true"  | "True"                                                      { TRUE }
	| "⊥" | "false" | "False"                                                     { FALSE }

	| "unit" | "Unit"                                                             { UTYPE }
	| "int"  | "Int"                                                              { ITYPE }
	| "list" | "List"                                                             { LTYPE }
	| "pair" | "Pair"                                                             { PTYPE }
	| "->"   | "→" | "to" | "To"                                                  { FUNTYPE }
	| "\\l"  | "λ" | "lambda" | "Lambda" | "function" | "Function"                { LAMBDA }

	| "list.map"    | "List.Map"                                                 	{ MAP }
	| "list.fold"   | "List.Fold"                                          				{ FOLD }
	| "list.filter" | "List.Filter"                                          			{ FILTER }
	| "list.limit"  | "List.Limit"	                                              { LIMIT }
	| "list.length" | "List.Length" | "#"                                         { LENGTH }

	| "list.rev"    | "List.Rev"                                                  { STRING_REV }

	| "let"       | "Let"                                                         { LET }
	| "unbind"    | "Unbind"                                                      { UNBIND }
	| "in"        | "In"                                                          { IN }
	| "if"        | "If"                                                          { IF }
	| "then"      | "Then"                                                        { THEN }
	| "else"      | "Else"                                                        { ELSE }
	| "match"     | "Match"                                                       { MATCH }
	| "while"     | "While"                                                       { WHILE }
	| "done"      | "Done"                                                        { DONE }
	| "do"        | "Do"                                                          { DO }
	| "loop"      | "Loop"                                                        { LOOP }
	| "for"       | "For"                                                         { FOR }
	| "done"      | "Done"                                                        { DONE }
	| "break"     | "Break"                                                       { BREAK }
	| "continue"  | "Continue"                                                    { CONTINUE }
	| "return"    | "Return"                                                      { RETURN }
	| "assert"    | "Assert"                                                      { ASSERT }
	| "exit"      | "Exit"                                                        { EXIT }
	| "cons"      | "Cons"                                                        { CONS }
	| "head"      | "Head"                                                        { HEAD }
	| "tail"      | "Tail"                                                        { TAIL }

	| "string.lower"           | "String.Lower"                                   { STRING_LOWER }
	| "string.upper"           | "String.Upper"                                   { STRING_UPPER }
	| "string.rev"             | "String.Rev"                                     { STRING_REV }

	| "io.readi"               | "IO.ReadI"
	| "console.read_int"       | "Console.Read_Int"                               { READ_INT }
	| "io.reads"               | "IO.ReadS"
	| "console.read_string"    | "Console.Read_String"                            { READ_STRING }
	| "io.readb"               | "IO.ReadB"
	| "console.read_bool"      | "Console.Read_Bool"                              { READ_BOOL }

	| "io.clear"               | "IO.Clear"
	| "console.clear"          | "Console.Clear"                                  { CLEAR }

	| "io.printi"              | "IO.PrintI"
	| "console.print_int"      | "Console.Print_Int"                              { PRINT_INT }
	| "io.prints"              | "IO.PrintS"
	| "console.print_string"   | "Console.Print_String"                           { PRINT_STRING }
	| "io.printb"              | "IO.PrintB"
	| "console.print_bool"     | "Console.Print_Bool"                             { PRINT_BOOL }

	| "io.printlni"            | "IO.PrintlnI"
	| "console.println_int"    | "Console.Println_Int"                            { PRINTLN_INT }
	| "io.printlns"            | "IO.PrintlnS"
	| "console.println_string" | "Console.Println_String"                         { PRINTLN_STRING }
	| "io.printlnb"            | "IO.PrintlnB"
	| "console.println_bool"   | "Console.Println_Bool"                           { PRINTLN_BOOL }

	| "io.errori"              | "IO.ErrorI"
	| "console.error_int"      | "Console.Error_Int"                              { ERROR_INT }
	| "io.errors"              | "IO.ErrorS"
	| "console.error_string"   | "Console.Error_String"                           { ERROR_STRING }
	| "io.errorb"              | "IO.ErrorB"
	| "console.error_bool"     | "Console.Error_Bool"                             { ERROR_BOOL }

	| "io.errorlni"            | "IO.ErrorlnI"
	| "console.errorln_int"    | "Console.Errorln_Int"                            { ERRORLN_INT }
	| "io.errorlns"            | "IO.ErrorlnS"
	| "console.errorln_string" | "Console.Errorln_String"                         { ERRORLN_STRING }
	| "io.errorlnb"            | "IO.ErrorlnB"
	| "console.errorln_bool"   | "Console.Errorln_Bool"                           { ERRORLN_BOOL }

	(* Brackets *)
	| '{'                                                                         { CURLYL }
	| '}'                                                                         { CURLYR }
	| '('                                                                         { ROUNDL }
	| ')'                                                                         { ROUNDR }
	| '['                                                                         { SQUAREL }
	| ']'                                                                         { SQUARER }
	| "⟦"                                                                         { DOUBLESQUAREL }
	| "⟧"                                                                         { DOUBLESQUARER }

	| ':'                                                                         { COLON }
	| '?'                                                                         { QUESTION }

	| '!'  | "¬" | '~' | "math.not" | "Math.Not"                                  { UNARY_NEGATION }

	| '-'  | "math.minus" | "Math.Minus"                                          { MINUS }
	| '+'  | "math.plus"  | "Math.Plus"                                           { PLUS }

	| '^'  | "math.pow"   | "Math.Pow"                                            { BINARY_POWER }
	| '*'  | "math.mul"   | "Math.Mul"                                            { BINARY_MULTIPLY }
	| '/'  | "math.div"   | "Math.Div"                                            { BINARY_DIVIDE }
	| '%'  | "math.mod"   | "Math.Mod"                                            { BINARY_MODULO }

	| "<<" | "math.left"  | "Math.Left"                                           { BITWISE_LEFT }
	| ">>" | "math.right" | "Math.Right"                                          { BITWISE_RIGHT }

	| "math.rand"   | "Math.Rand"
	| "math.random" | "Math.Random"                                               { RANDOM }
	| "math.min"    | "Math.Min"                                                  { MATH_MIN }
	| "math.max"    | "Math.Max"                                                  { MATH_MAX }
	| "math.abs"    | "Math.Abs"                                                  { MATH_ABS }
	| "math.sign"   | "Math.Sign"                                                 { MATH_SIGN }
	| "math.sqrt"   | "Math.Sqrt"                                                 { MATH_SQRT }
	| "math.ln"     | "Math.Ln"                                                   { MATH_LN }
	| "math.log"    | "Math.Log"                                                  { MATH_LOG }
	| "math.fact"   | "Math.Fact"                                                 { MATH_FACT }

	| '<'                                                                         { COMPARE_LT }
	| "≤" | "<="                                                                  { COMPARE_LTE }
	| '>'                                                                         { COMPARE_GT }
	| "≥" | ">="                                                                  { COMPARE_GTE }
	| "=="                                                                        { COMPARE_E }
	| "≠" | "!="                                                                  { COMPARE_NE }

	| '&' | "∧" | "math.and" | "Math.And"                                         { BITWISE_AND }
	| '^' | "⊻" | "math.xor" | "Math.XOr"                                         { BITWISE_XOR }
	| '|' | "∨" | "⊕"  | "math.or" | "Math.Or"                                    { BITWISE_OR }

	| "+="                                                                        { ASSIGN_ADDITION }
	| "-="                                                                        { ASSIGN_SUBTRACT }
	| "*="                                                                        { ASSIGN_MULTIPLY }
	| "/="                                                                        { ASSIGN_DIVIDE }
	| "%="                                                                        { ASSIGN_MODULO }
	| "&="                                                                        { ASSIGN_AND }
	| "^="                                                                        { ASSIGN_XOR }
	| "|="                                                                        { ASSIGN_OR }
	| '='                                                                         { ASSIGN_EQUAL }

	(* Need to be after keywords *)
	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''-''_']* as v                         { IDENT v }
	|            ['0'-'9''_']+ as v
	| '0'['b''B']['0'-'1''_']+ as v
	| '0'['o''O']['0'-'7''_']+ as v
	| '0'['x''X']['0'-'9''a'-'f''_']+ as v                                        { INT (int_of_string (Str.global_replace (Str.regexp "_") "" v)) }
	| '\'' [^ '\''] '\'' as v                                                     { INT (Char.code v.[1])}
	| '"'                                                                         { STRING ( read_string (Buffer.create 100) lexbuf ) }

	| '.'                                                                         { DOT }
	| ','                                                                         { COMMA }
	| ';'                                                                         { SEMI_COLON }
	| eof                                                                         { EOF }

	(* catch all pattern *)
	| _                      { lex_error lexbuf }

and read_string buf = parse
	| '"'                                                                         { Buffer.contents buf }
	| '\\' '"'                                                                    { Buffer.add_char buf '"'; read_string buf lexbuf }
	| '\\' '/'                                                                    { Buffer.add_char buf '/'; read_string buf lexbuf }
	| '\\' '\\'                                                                   { Buffer.add_char buf '\\'; read_string buf lexbuf }
	| '\\' 'b'                                                                    { Buffer.add_char buf '\b'; read_string buf lexbuf }
	| '\\' 'f'                                                                    { Buffer.add_char buf '\012'; read_string buf lexbuf }
	| '\\' 'n'                                                                    { Buffer.add_char buf '\n'; read_string buf lexbuf }
	| '\\' 'r'                                                                    { Buffer.add_char buf '\r'; read_string buf lexbuf }
	| '\\' 't'                                                                    { Buffer.add_char buf '\t'; read_string buf lexbuf }
	| [^ '"' '\\']+                                                               { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
	| _                                                                           { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
	| eof                                                                         { raise (SyntaxError ("String is not terminated!")) }

and read_block_comment buf = parse
	| '*'[' ''\t''\n''\r']*'/' | eof                                              { Buffer.contents buf }
	| _                                                                           { Buffer.add_string buf (Lexing.lexeme lexbuf); read_block_comment buf lexbuf}

and read_line_comment buf = parse
	| [^'\n''\r']*                                                                { Buffer.add_string buf (Lexing.lexeme lexbuf); read_line_comment buf lexbuf }
	| ['\n''\r'] | eof                                                            { Buffer.contents buf }
