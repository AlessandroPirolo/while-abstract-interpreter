{

(*open Ast*)
open Parser
exception Eof 

}

let whitespace = [' ' '\t' ]
let digit = ['0'-'9'] 
let int = ['-']?digit+
let idbody = ['a'-'z' 'A'-'Z' '0'-'9' '_']*['\'']*	
let Uid = ['A'-'Z'] idbody
let Lid = ['a'-'z' '_'] idbody
let id = Uid | Lid

rule tokenize = parse 
    whitespace    { tokenize lexbuf }  
    | eof            { EOF }
    
    | '+'			{ PLUS }
	| '-'			{ MINUS }
	| '*'			{ MULT }
    | '/'           { DIV }
    | ":="          { ASSIGN }
	| "!="			{ NEQ }
    | '='			{ EQ }
	| '<'			{ LT }
	| '>'			{ GT }
	| "<="			{ LEQ }
	| ">="			{ GEQ }
	| "||"			{ OR }
	| "&&"			{ AND }
	| "!"			{ NOT }
    | "{"           { START_STMT }
    | "}"           { END_STMT }
    | ";"           { COMP }
    | "++"          { INC }
    | "--"          { DEC }

    (* keywords *)
    | "if"          { IF }
    | "then"        { THEN }
    | "else"		{ ELSE }
	| "true"		{ TRUE }
	| "false"		{ FALSE }
    | "while"       { WHILE }
    | "do"          { DO }
    | "skip"        { SKIP }

	(* identifiers *)
	| id    { STRING (Lexing.lexeme lexbuf) }
    | int   { INT (int_of_string (Lexing.lexeme lexbuf)) }
    