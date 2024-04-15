module Ast

(* Abstract syntax tree for arithmetic expressions *)
type Aexpr =
  | AConst of int
  | Var of string
  | ABinOp of Aexpr * string * Aexpr
  | Neg of string
  with member this.ToString = 
        match this with
        | AConst n -> n.ToString()
        | Var s -> s
        | ABinOp (a1, s, a2) ->
            "ABinOp (\n\t" + a1.ToString + ", " + s + ", \n\t" + a2.ToString
            + "\n\t)"
        | Neg s -> "-" + s

(* Abstract syntax tree for boolean expressions *)
type Bexpr =
  | BConst of bool
  | BUnOp of string * Bexpr
  | BBinOp of Bexpr * string * Bexpr
  | BoolRelation of Aexpr * string * Aexpr
  with member this.ToString =
        match this with
        | BConst true -> "true"
        | BConst false -> "false"
        | BUnOp (s, b) -> "BUnOp (" + s + ", " + b.ToString + ")"
        | BBinOp (b1, s, b2) ->
            "BBinOp (\n\t" + b1.ToString + ", " + s + ", \n\t" + b2.ToString
            + "\n\t)"
        | BoolRelation (b1, s, b2) ->
            "BoolRelation (\n\t" + b1.ToString + ", " + s + ", \n\t"
            + b2.ToString + "\n\t)"

(* Abstract syntax tree for the operations *)
type Statement =
  | Assignment of string * Aexpr
  | Skip
  | Composition of Statement * Statement
  | Conditional of Bexpr * Statement * Statement
  | While of Bexpr * Statement
  | IncDec of string * string
  with member this.ToString =
        match this with
        | Assignment (s, a) -> "Assignment (" + s + ", " + a.ToString + ")"
        | Skip -> "Skip"
        | Composition (e1, e2) ->
            "Composition ( \n\t" + e1.ToString + ", \n\t" + e2.ToString + "\n\t)"
        | Conditional (b, e1, e2) ->
            "Conditional (" + b.ToString + ", \n\t" + e1.ToString + ", \n\t"
            + e2.ToString + "\n\t)"
        | While (b, s) ->
            "While (" + b.ToString + ", \n\t" + s.ToString + "\n\t)"
        | IncDec (s, a) -> "IncDec (" + s + ", " + a + ")"
