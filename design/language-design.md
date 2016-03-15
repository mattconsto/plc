Flow Programming Language
=========================

# General

* Default signed, have to use unsigned
* case insensitive
* UTF-8 everywhere
* Pointers
* Optional semi colons
* forced spacing
* no brackets
* json and xml and yaml and csv and etc. support?
* can use symbols such as λ → instead of lambda and ->
* top level repl loop
* regex
* very important to be turing complete!!!!!
* concurrency?
* lazy eval?
* left to right?
* strong static with polymorphism.
* classes and inerfaces?
* IO?
* expections, try catch finally?
* important to be able to access collections as streams

# Code

if condition:
	if false:
		then do_something()
	else:
		loop_forever()

# Grammar

assignment        ::= "let" <variable> [ { <name> } ] ":=" ( <name> | <type> );
name              ::= ( * Any UTF-8 character expect 0 - 9 * ) [ { <data_character> } ];

type              ::= <type_bool> | <type_number> | <type_collection> | <type_function>;
	type_ident      ::= "bool" | "numb" | "char" | "int" | "float" | "collection" | "stream" | "record" | "list" | "string" | "map" | "set" | "variant" | "function";

	type_bool       ::= "true" | "false";

	type_number     ::= [ "-" ] ( <type_char> | <type_int> | <type_float> );
		type_char     ::= <data_number> [ "b" ] | ' <data_character> ';
		type_int      ::= <data_number> [ "i" | "l" ];
		type_float    ::= <data_number> [ "." [ <data_number> ] ] [ "f" | "d" ];

	type_collection ::= <type_stream> | <type_record> | <type_list> | <type_map> | <type_set> | <type_variant>;
		type_stream   ::= "<" <type> "," <type_function> ">";
		type_record   ::= "(" <type> [ { "," <type> } ] ")" | "(" <name> ":" <type> [ { "," <name> ":" <type> } ] ")";
		type_list     ::= "[" <type> [ { "," <type> } ] "]";
			type_string ::= """ [ { <data_character> } ] """;
		type_map      ::= "{" <name> ":" <type> [ { "," <name> ":" <type> } ] "}";
		type_set      ::= ;
		type_variant  ::= <name> [ "|" <name> ];

	type_function   ::= ;

data_number     ::= ( <data_number_b2> | <data_number_b8> | <data_number_b10> | <data_number_b16> );
data_number_b2  ::= 0 ( "b" | "B" ) { <data_digit_base2> };
data_number_b8  ::= 0 ( "q" | "Q" ) { <data_digit_base8> };
data_number_b10 ::=   [ "d" | "d" ] { <data_digit_b10> };
data_number_b16 ::= 0 ( "x" | "X" ) { <data_digit_b16> };

data_digit_b2   ::= 0 | 1;
data_digit_b8   ::= <data_digit_b2>  | 2 | 3 | 4 | 5 | 6 | 7;
data_digit_b10  ::= <data_digit_b8>  | 8 | 9;
data_digit_b16  ::= <data_digit_b10> | "a" | "b" | "c" | "d" | "e" | "f";

data_character  ::= ( * Any UTF-8 character * );

# Namespaces

import flow.types.collection.record.url

Like java, except you can collapse directorys, more expanded from the left takes prescendece.

flow > collection > record > url
flow > collection.record > url

flow auto imported

# Data Types

* Optional
* Boolean
* Number
	* Char
	* Int
	* Float
* Collection
	* Stream
	* Record
		* Complex
		* URL
	* List
		* Array
			* String
		* Double Linked
	* Map
	* Set
	* DeQueue
		* Priority DeQueue
	* Stack
	* Enumerable
	* Graph
		* Tree
		* More?
* Function

# Standard library

flow
	number
	collection
	function
	math
	file
