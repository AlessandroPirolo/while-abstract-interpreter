module Eval

open Ast
open EvalBexpr
open AbstractState
open EvalAexpr
open Interval

exception Error of string

let rec eval (stmt : Statement) (state : State) (states : State list) : State =
  match stmt with
  | Skip -> state
  | Composition (expr1, expr2) ->
      let state1 = eval expr1 state states
      eval expr2 state1 (states @ [ state1 ])
  | Assignment (str, aexpr) ->
      let res = eval_aexpr aexpr state 
      let new_state = add str res state
      
      new_state

  | Conditional (cond, expr1, expr2) ->
      let true_cond = eval_bexpr cond state 
      let false_cond = eval_bexpr (BUnOp ("!", cond)) state 
      let t = if isEmpty true_cond then Map.empty else eval expr1 true_cond [] 
      let f = if isEmpty false_cond then Map.empty else eval expr2 false_cond [] 
      
      union t f
      
  | IncDec (e, op) -> 
      let value = find e state 
      match op with
      | "++" -> add e (value.AbstractInc) state
      | "--" -> add e (value.AbstractDec) state
      | _ -> state
     
  | While (bexpr, expr) -> 
      (* B[-b](lfp(fun x -> s V (D[S]oB[b])x )) *)
      let b = eval_bexpr bexpr state
      let not_b = eval_bexpr (BUnOp("!", bexpr)) state 
      let mutable fixpoint = false 
      let mutable prev_state = eval expr b []  
      let mutable succ_state = prev_state 
      while not fixpoint do 
        let nb = eval_bexpr bexpr prev_state 
        succ_state <- eval expr nb []
        let un = union prev_state succ_state
        let wide = widening prev_state un
        if wide = succ_state then
          fixpoint <- true
        else
          prev_state <- succ_state

      eval expr not_b []
