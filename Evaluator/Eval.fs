module Eval

open Ast
open EvalBexpr
open AbstractState
open EvalAexpr

exception Error of string

let rec eval (stmt : Statement) (state : State) (states : State list) : State * State list =
  match stmt with
  | Skip -> (state, states)
  | Composition (expr1, expr2) ->
      let state1, states1 = eval expr1 state states
      let state2, states2 = eval expr2 state1 states1
      (state2, states2)
  | Assignment (str, aexpr) ->
      let res = eval_aexpr aexpr state 
      let new_state = add str res state
      
      (new_state, (states @ [new_state]))
  | Conditional (cond, expr1, expr2) ->
      let true_cond = eval_bexpr cond state 
      let false_cond = eval_bexpr (BUnOp ("!", cond)) state 
      let t, true_branch_state = if isEmpty true_cond then (Map.empty, []) else eval expr1 true_cond [] 
      let f, false_branch_state = if isEmpty false_cond then (Map.empty, []) else eval expr2 false_cond [] 
      
      (union t f, states @ true_branch_state @ false_branch_state)
      
  | IncDec (e, op) -> 
      let value = find e state 
      let new_state = 
        match op with
        | "++" -> add e (value.AbstractInc) state
        | "--" -> add e (value.AbstractDec) state
        | _ -> state

      (new_state, states @ [new_state])
     
  | While (bexpr, expr) -> 
      (* B[-b](lfp(fun x -> s V (D[S]oB[b])x )) *)
      let b = eval_bexpr bexpr state
      printfn "b is %s" (to_string b)
      let mutable fixpoint = false 
      let mutable prev_state = state  
      let mutable succ_state, _ = eval expr b []
      while not fixpoint do 
        let un = union prev_state succ_state
        printfn "un is %s" (to_string un)
        let wide = widening prev_state un
        printfn "wide is %s" (to_string wide)
        let nb = eval_bexpr bexpr succ_state
        succ_state <- fst(eval expr nb [])
        if wide = un then
          fixpoint <- true
        else
          prev_state <- wide

      printfn "succ state %s" (to_string prev_state)
      (eval_bexpr (BUnOp("!", bexpr)) prev_state, [])
