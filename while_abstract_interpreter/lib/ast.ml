module Ast = struct
  (* Abstract syntax tree for arithmetic expressions *)
  type aexpr =
    | AConst of int
    | Var of string
    | ABinOp of aexpr * string * aexpr
    | Neg of string

  (* Abstract syntax tree for boolean expressions *)
  type bexpr =
    | BConst of bool
    | BUnOp of string * bexpr
    | BBinOp of bexpr * string * bexpr
    | BoolRelation of aexpr * string * aexpr

  (* Abstract syntax tree for the operations *)
  type statement =
    | Assignment of string * aexpr
    | Skip
    | Composition of statement * statement
    | Conditional of bexpr * statement * statement
    | While of bexpr * statement
    | IncDec of string * string

  let rec to_string_a = function
    | AConst n -> string_of_int n
    | Var s -> s
    | ABinOp (a1, s, a2) ->
        "ABinOp (\n\t" ^ to_string_a a1 ^ ", " ^ s ^ ", \n\t" ^ to_string_a a2
        ^ "\n\t)"
    | Neg s -> "-" ^ s

  let rec to_string_b = function
    | BConst true -> "true"
    | BConst false -> "false"
    | BUnOp (s, b) -> "BUnOp (" ^ s ^ ", " ^ to_string_b b ^ ")"
    | BBinOp (b1, s, b2) ->
        "BBinOp (\n\t" ^ to_string_b b1 ^ ", " ^ s ^ ", \n\t" ^ to_string_b b2
        ^ "\n\t)"
    | BoolRelation (b1, s, b2) ->
        "BoolRelation (\n\t" ^ to_string_a b1 ^ ", " ^ s ^ ", \n\t"
        ^ to_string_a b2 ^ "\n\t)"

  let rec to_string = function
    | Assignment (s, a) -> "Assignment (" ^ s ^ ", " ^ to_string_a a ^ ")"
    | Skip -> "Skip"
    | Composition (e1, e2) ->
        "Composition ( \n\t" ^ to_string e1 ^ ", \n\t" ^ to_string e2 ^ "\n\t)"
    | Conditional (b, e1, e2) ->
        "Conditional (" ^ to_string_b b ^ ", \n\t" ^ to_string e1 ^ ", \n\t"
        ^ to_string e2 ^ "\n\t)"
    | While (b, s) ->
        "While (" ^ to_string_b b ^ ", \n\t" ^ to_string s ^ "\n\t)"
    | IncDec (s, a) -> "IncDec (" ^ s ^ ", " ^ a ^ ")"
end
