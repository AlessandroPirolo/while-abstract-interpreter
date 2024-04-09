%{
    open Ast
%}

%token <int> INT
%token <string> STRING

%token IF THEN ELSE WHILE DO FOR SKIP TO
       TRUE FALSE 
       START_STMT END_STMT
       PLUS MINUS MULT DIV ASSIGN INCEQ DECEQ MULTEQ
       LT GT LEQ GEQ EQ NEQ INC DEC
       AND OR NOT
       COMP
%token EOF

%type < Ast.statement > prog

%start prog 

%%

prog:
    seq_expr EOF                           { $1 }

seq_expr
    : statement                                      { $1 }
    | START_STMT seq_expr END_STMT              { $2 }
    | seq_expr COMP seq_expr                    { Ast.Composition ($1, $3) }


statement
    : IF bexpr THEN statement ELSE statement              { Ast.Conditional ($2, $4, $6) }
    | WHILE bexpr DO statement                       { Ast.While ($2, $4) }
    | STRING ASSIGN aexpr                            { Ast.Assignment ($1, $3) }
    | SKIP                                      { Ast.Skip }
    | START_STMT seq_expr END_STMT              { $2 }

bexpr
    : bexpr AND bexpr               { Ast.BBinOp ($1, "&&", $3) }
    | bexpr OR bexpr                { Ast.BBinOp ($1, "||", $3) }
    | NOT bexpr                     { Ast.BUnOp ("!", $2) }
    | TRUE                          { Ast.BConst (true) }
    | FALSE                         { Ast.BConst (false) }
    | aexpr LEQ aexpr               { Ast.BoolRelation ($1, "<=", $3) }
    | aexpr GEQ aexpr               { Ast.BoolRelation ($1, ">=", $3) }
    | aexpr EQ aexpr                { Ast.BoolRelation ($1, "=", $3) }
    | aexpr LT aexpr                { Ast.BoolRelation ($1, "<", $3) }
    | aexpr GT aexpr                { Ast.BoolRelation ($1, ">", $3) }
    | aexpr NEQ aexpr               { Ast.BoolRelation ($1, "!=", $3) }
    | START_STMT bexpr END_STMT     { $2 }

aexpr
    : aexpr PLUS aexpr              { ABinOp ($1, "+", $3) }
    | aexpr MINUS aexpr             { ABinOp ($1, "-", $3) }
    | aexpr MULT aexpr              { ABinOp ($1, "*", $3) }
    | aexpr DIV aexpr               { ABinOp ($1, "/", $3) }
    | STRING INC                    { IncDec ($1, "++")}
    | STRING DEC                    { IncDec ($1, "--")}
    | MINUS STRING                  { Neg ($2) }
    | INT                           { AConst ($1) }
    | STRING                        { Var ($1) }
    | START_STMT aexpr END_STMT     { $2 }

