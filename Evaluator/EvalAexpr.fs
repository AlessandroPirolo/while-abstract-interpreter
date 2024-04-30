module EvalAexpr

open AbstractState
open Interval
open Number
open Ast

let rec eval_aexpr (expr : Aexpr) (state : State) : Interval =
  match expr with
  | AConst n -> Interval (Num n, Num n)
  | Var s -> find s state
  | Neg s -> - (eval_aexpr s state)
  | ABinOp (op1, e, op2) -> 
      let f = eval_aexpr op1 state
      let s = eval_aexpr op2 state
      match e with
      | "+" -> f + s
      | "-" -> f - s
      | "*" -> f * s
      | "/" -> f / s
      | _ -> raise (Error "Unknown operation!")