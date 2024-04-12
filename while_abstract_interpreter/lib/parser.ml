type token =
  | INT of (
# 5 "parser.mly"
        int
# 6 "parser.ml"
)
  | STRING of (
# 6 "parser.mly"
        string
# 11 "parser.ml"
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

open Parsing
let _ = parse_error;;
# 2 "parser.mly"
    open Ast
# 51 "parser.ml"
let yytransl_const = [|
  259 (* IF *);
  260 (* THEN *);
  261 (* ELSE *);
  262 (* WHILE *);
  263 (* DO *);
  264 (* FOR *);
  265 (* SKIP *);
  266 (* TO *);
  267 (* TRUE *);
  268 (* FALSE *);
  269 (* START_STMT *);
  270 (* END_STMT *);
  271 (* PLUS *);
  272 (* MINUS *);
  273 (* MULT *);
  274 (* DIV *);
  275 (* ASSIGN *);
  276 (* INCEQ *);
  277 (* DECEQ *);
  278 (* MULTEQ *);
  279 (* LT *);
  280 (* GT *);
  281 (* LEQ *);
  282 (* GEQ *);
  283 (* EQ *);
  284 (* NEQ *);
  285 (* INC *);
  286 (* DEC *);
  287 (* AND *);
  288 (* OR *);
  289 (* NOT *);
  290 (* COMP *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\006\000\004\000\003\000\001\000\
\003\000\002\000\002\000\003\000\003\000\002\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\008\000\000\000\032\000\
\000\000\002\000\000\000\010\000\011\000\029\000\030\000\015\000\
\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\028\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
\000\000\023\000\031\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\000\000\000\000\009\000\005\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\021\000\022\000"

let yysindex = "\008\000\
\138\255\000\000\087\255\013\255\013\255\000\000\138\255\000\000\
\003\000\000\000\092\255\000\000\000\000\000\000\000\000\000\000\
\000\000\013\255\009\255\013\255\055\255\111\255\036\255\250\254\
\138\255\000\000\092\255\115\255\067\255\097\255\000\000\247\254\
\140\255\013\255\013\255\092\255\092\255\092\255\092\255\092\255\
\092\255\092\255\092\255\092\255\092\255\140\255\000\000\235\254\
\086\255\000\000\000\000\138\255\015\255\247\254\247\254\115\255\
\115\255\115\255\115\255\115\255\115\255\115\255\115\255\115\255\
\115\255\000\000\040\255\140\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\003\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\000\000\000\000\000\000\000\000\000\000\141\255\150\255\001\000\
\016\000\047\000\062\000\044\255\046\255\057\255\059\255\065\255\
\078\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\249\255\229\255\022\000\250\255"

let yytablesize = 352
let yytable = "\024\000\
\024\000\007\000\026\000\004\000\028\000\053\000\014\000\047\000\
\001\000\014\000\031\000\030\000\025\000\014\000\015\000\025\000\
\014\000\048\000\066\000\068\000\049\000\034\000\035\000\016\000\
\017\000\018\000\023\000\025\000\019\000\056\000\057\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\029\000\
\070\000\032\000\046\000\000\000\067\000\020\000\026\000\020\000\
\000\000\021\000\020\000\000\000\021\000\069\000\000\000\054\000\
\055\000\020\000\033\000\021\000\017\000\027\000\018\000\017\000\
\000\000\018\000\034\000\035\000\019\000\000\000\017\000\019\000\
\018\000\025\000\020\000\020\000\021\000\021\000\019\000\000\000\
\050\000\022\000\000\000\000\000\022\000\034\000\035\000\017\000\
\017\000\018\000\018\000\022\000\014\000\015\000\000\000\019\000\
\019\000\034\000\035\000\051\000\036\000\037\000\038\000\039\000\
\027\000\011\000\000\000\019\000\022\000\022\000\051\000\036\000\
\037\000\038\000\039\000\012\000\013\000\000\000\000\000\040\000\
\041\000\042\000\043\000\044\000\045\000\036\000\037\000\038\000\
\039\000\036\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\003\000\004\000\003\000\004\000\005\000\
\012\000\005\000\006\000\012\000\006\000\000\000\007\000\000\000\
\052\000\013\000\012\000\000\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\024\000\024\000\007\000\024\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\000\007\000\
\000\000\004\000\000\000\025\000\025\000\000\000\025\000\024\000\
\024\000\024\000\024\000\024\000\024\000\025\000\000\000\024\000\
\024\000\000\000\024\000\007\000\025\000\000\000\025\000\025\000\
\025\000\025\000\025\000\025\000\000\000\000\000\025\000\025\000\
\000\000\025\000\026\000\026\000\000\000\026\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
\000\000\027\000\027\000\000\000\027\000\026\000\026\000\026\000\
\026\000\026\000\026\000\027\000\000\000\026\000\026\000\000\000\
\026\000\000\000\000\000\000\000\027\000\027\000\027\000\027\000\
\027\000\027\000\000\000\000\000\027\000\027\000\000\000\027\000"

let yycheck = "\007\000\
\000\000\000\000\000\000\000\000\011\000\033\000\004\001\014\001\
\001\000\007\001\002\001\018\000\034\001\001\001\002\001\000\000\
\014\001\025\000\046\000\005\001\027\000\031\001\032\001\011\001\
\012\001\013\001\005\000\034\001\016\001\036\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\018\000\
\068\000\020\000\007\001\255\255\052\000\033\001\000\000\004\001\
\255\255\004\001\007\001\255\255\007\001\014\001\255\255\034\000\
\035\000\014\001\004\001\014\001\004\001\000\000\004\001\007\001\
\255\255\007\001\031\001\032\001\004\001\255\255\014\001\007\001\
\014\001\034\001\031\001\032\001\031\001\032\001\014\001\255\255\
\014\001\004\001\255\255\255\255\007\001\031\001\032\001\031\001\
\032\001\031\001\032\001\014\001\001\001\002\001\255\255\031\001\
\032\001\031\001\032\001\014\001\015\001\016\001\017\001\018\001\
\013\001\019\001\255\255\016\001\031\001\032\001\014\001\015\001\
\016\001\017\001\018\001\029\001\030\001\255\255\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\015\001\016\001\017\001\
\018\001\015\001\016\001\017\001\018\001\023\001\024\001\025\001\
\026\001\027\001\028\001\002\001\003\001\002\001\003\001\006\001\
\004\001\006\001\009\001\007\001\009\001\255\255\013\001\255\255\
\013\001\004\001\014\001\255\255\007\001\255\255\255\255\255\255\
\255\255\255\255\255\255\014\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\005\001\005\001\007\001\
\255\255\255\255\255\255\255\255\255\255\255\255\014\001\014\001\
\255\255\014\001\255\255\004\001\005\001\255\255\007\001\023\001\
\024\001\025\001\026\001\027\001\028\001\014\001\255\255\031\001\
\032\001\255\255\034\001\034\001\034\001\255\255\023\001\024\001\
\025\001\026\001\027\001\028\001\255\255\255\255\031\001\032\001\
\255\255\034\001\004\001\005\001\255\255\007\001\255\255\255\255\
\255\255\255\255\255\255\255\255\014\001\255\255\255\255\255\255\
\255\255\004\001\005\001\255\255\007\001\023\001\024\001\025\001\
\026\001\027\001\028\001\014\001\255\255\031\001\032\001\255\255\
\034\001\255\255\255\255\255\255\023\001\024\001\025\001\026\001\
\027\001\028\001\255\255\255\255\031\001\032\001\255\255\034\001"

let yynames_const = "\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  FOR\000\
  SKIP\000\
  TO\000\
  TRUE\000\
  FALSE\000\
  START_STMT\000\
  END_STMT\000\
  PLUS\000\
  MINUS\000\
  MULT\000\
  DIV\000\
  ASSIGN\000\
  INCEQ\000\
  DECEQ\000\
  MULTEQ\000\
  LT\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  EQ\000\
  NEQ\000\
  INC\000\
  DEC\000\
  AND\000\
  OR\000\
  NOT\000\
  COMP\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'seq_expr) in
    Obj.repr(
# 24 "parser.mly"
                                           ( _1 )
# 285 "parser.ml"
               :  Ast.statement ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 27 "parser.mly"
                                                     ( _1 )
# 292 "parser.ml"
               : 'seq_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'seq_expr) in
    Obj.repr(
# 28 "parser.mly"
                                                ( _2 )
# 299 "parser.ml"
               : 'seq_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'seq_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'seq_expr) in
    Obj.repr(
# 29 "parser.mly"
                                                ( Ast.Composition (_1, _3) )
# 307 "parser.ml"
               : 'seq_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'bexpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 33 "parser.mly"
                                                          ( Ast.Conditional (_2, _4, _6) )
# 316 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'bexpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 34 "parser.mly"
                                                     ( Ast.While (_2, _4) )
# 324 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 35 "parser.mly"
                                                     ( Ast.Assignment (_1, _3) )
# 332 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                                                ( Ast.Skip )
# 338 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'seq_expr) in
    Obj.repr(
# 37 "parser.mly"
                                                ( _2 )
# 345 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 38 "parser.mly"
                                    ( Ast.IncDec (_1, "++"))
# 352 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 39 "parser.mly"
                                    ( Ast.IncDec (_1, "--"))
# 359 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexpr) in
    Obj.repr(
# 42 "parser.mly"
                                    ( Ast.BBinOp (_1, "&&", _3) )
# 367 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexpr) in
    Obj.repr(
# 43 "parser.mly"
                                    ( Ast.BBinOp (_1, "||", _3) )
# 375 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexpr) in
    Obj.repr(
# 44 "parser.mly"
                                    ( Ast.BUnOp ("!", _2) )
# 382 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                                    ( Ast.BConst (true) )
# 388 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                                    ( Ast.BConst (false) )
# 394 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 47 "parser.mly"
                                    ( Ast.BoolRelation (_1, "<=", _3) )
# 402 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 48 "parser.mly"
                                    ( Ast.BoolRelation (_1, ">=", _3) )
# 410 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 49 "parser.mly"
                                    ( Ast.BoolRelation (_1, "=", _3) )
# 418 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 50 "parser.mly"
                                    ( Ast.BoolRelation (_1, "<", _3) )
# 426 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 51 "parser.mly"
                                    ( Ast.BoolRelation (_1, ">", _3) )
# 434 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 52 "parser.mly"
                                    ( Ast.BoolRelation (_1, "!=", _3) )
# 442 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bexpr) in
    Obj.repr(
# 53 "parser.mly"
                                    ( _2 )
# 449 "parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 56 "parser.mly"
                                    ( Ast.ABinOp (_1, "+", _3) )
# 457 "parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 57 "parser.mly"
                                    ( Ast.ABinOp (_1, "-", _3) )
# 465 "parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 58 "parser.mly"
                                    ( Ast.ABinOp (_1, "*", _3) )
# 473 "parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 59 "parser.mly"
                                    ( Ast.ABinOp (_1, "/", _3) )
# 481 "parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                                    ( Ast.Neg (_2) )
# 488 "parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 61 "parser.mly"
                                    ( Ast.AConst (_1) )
# 495 "parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                                    ( Ast.Var (_1) )
# 502 "parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'aexpr) in
    Obj.repr(
# 63 "parser.mly"
                                    ( _2 )
# 509 "parser.ml"
               : 'aexpr))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Ast.statement )
