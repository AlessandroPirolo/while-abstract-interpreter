type token =
  | INT of (
# 5 "parser.mly"
        int
# 6 "parser.mli"
)
  | STRING of (
# 6 "parser.mly"
        string
# 11 "parser.mli"
)
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | FOR
  | SKIP
  | TO
  | TRUE
  | FALSE
  | START_STMT
  | END_STMT
  | PLUS
  | MINUS
  | MULT
  | DIV
  | ASSIGN
  | INCEQ
  | DECEQ
  | MULTEQ
  | LT
  | GT
  | LEQ
  | GEQ
  | EQ
  | NEQ
  | INC
  | DEC
  | AND
  | OR
  | NOT
  | COMP
  | EOF

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Ast.Ast.statement 
