module Eval

open Ast
open EvalBexpr
open AbstractState
open EvalAexpr

exception Error of string

let rec eval (stmt : Statement) (state : State) (states : State list) : State =
  match stmt with
  | Skip -> state
  | Composition (expr1, expr2) ->
      let state1 = eval expr1 state states in
      eval expr2 state1 (states @ [ state1 ])
  | Assignment (str, aexpr) ->
      let res = eval_aexpr aexpr state in
      let new_state = add str res state in
      new_state
  | Conditional (cond, expr1, expr2) ->
      let true_cond = eval_bexpr cond state in
      let false_cond = eval_bexpr (Ast.BUnOp ("!", cond)) state in

      let t = eval expr1 true_cond [] in

      let f = eval expr2 false_cond [] in

      //union t f
      t
  | IncDec (op, e) -> (
      let value = find e state in
      match op with
      | "++" -> add e (value.AbstractInc) state
      | "--" -> add e (value.AbstractDec) state
      | _ -> state
     )
  | While (_, _) -> 
      (* B[-b](lfp(fun x -> s V (D[S]oB[b])x )) *)
      (*let b = eval_bexpr cond state in
      (*let not_b = eval_bexpr (BUnOp("!", cond)) state in *)
      let fixpoint = ref false in
      let prev = ref (eval_bexpr cond b) in
      let succ = ref (eval_bexpr cond b) in
      while not fixpoint.contents do 
        let res = eval expr prev.contents [] in
        (*succ := ref (eval_bexpr cond res*) 
        let un = union prev.contents succ.contents in
        let wide = widening prev.contents un in
        if wide = succ.contents then
          fixpoint := true
        else
          prev := succ.contents
      done;*)
      state
