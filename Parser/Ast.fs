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
            a1.ToString +  s  + a2.ToString
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
        | BUnOp (s, b) -> "!" + b.ToString
        | BBinOp (b1, s, b2) ->
            b1.ToString + s + b2.ToString
        | BoolRelation (b1, s, b2) ->
            b1.ToString  + s + b2.ToString

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
        | Assignment (s, a) -> s + " := " + a.ToString
        | Skip -> "skip"
        | Composition (e1, e2) ->
            e1.ToString + ";\n" + e2.ToString
        | Conditional (b, e1, e2) ->
            "if " + b.ToString + " then \n\t " + e1.ToString + "\nelse \n\t"
            + e2.ToString + "\n"
        | While (b, s) ->
            "while " + b.ToString + " do " + s.ToString + "\n"
        | IncDec (e, op) -> e + op 
