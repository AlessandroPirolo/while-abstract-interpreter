module Eval

open Ast
open AbstractState
open IntervalDomain

exception Error of string

let rec eval (stmt : Statement) (state : State) (domain : IntervalDomain) : State * State Option list =
  match stmt with
  | Skip -> (state, [])
  | Composition (expr1, expr2) ->
      let state1, inv1 = eval expr1 state domain
      let state2, inv2 = eval expr2 state1 domain
      (state2, inv1 @ inv2)
  | Assignment (str, aexpr) ->
      let res = domain.eval_aexpr aexpr state 
      let new_state = add str res state
      
      (new_state, [])
  | Conditional (cond, expr1, expr2) ->
      let true_cond = domain.eval_bexpr cond state 
      let false_cond = domain.eval_bexpr (BUnOp ("!", cond)) state 
      let t, invf = if isEmpty true_cond then (Map.empty, []) else eval expr1 true_cond domain
      let f, invt = if isEmpty false_cond then (Map.empty, []) else eval expr2 false_cond domain
      
      (union t f, invt @ invf)
      
  | IncDec (e, op) -> 
      let value = find e state 
      let new_state = add e (domain.eval_incdec value op) state

      (new_state, [])
     
  | While (bexpr, expr) -> 
      (* B[-b](lfp(fun x -> s V (D[S]oB[b])x )) *)
      let mutable fixpoint = false 
      let mutable fixpoint_nar = false
      let mutable curr = state 
      let mutable succ = Map.empty
      let mutable inv : State Option = Some Map.empty

      while not fixpoint do 
        let b = domain.eval_bexpr bexpr curr
        inv <- Some b
        let eval_expr, _ = eval expr b domain
        succ <- eval_expr
        let un = union curr succ
        let wide = widening1 curr un

        if wide = un then
          fixpoint <- true
        else
          curr <- wide
          
      let exit_cond = domain.eval_bexpr (BUnOp("!", bexpr)) curr
      let nar = narrowing exit_cond succ

      (nar, [inv])
