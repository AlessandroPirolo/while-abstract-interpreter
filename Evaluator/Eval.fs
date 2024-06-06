module Eval

open Ast
open AbstractDomain

exception Error of string

let rec eval (stmt : Statement) (state : Map<string, 'T>) (domain : AbstractDomain<_>) (widening_delay : int) : Map<string, 'T> * Map<string, 'T> Option list =
  match stmt with
  | Skip -> (state, [])
  | Composition (expr1, expr2) ->
      let state1, inv1 = eval expr1 state domain widening_delay
      let state2, inv2 = eval expr2 state1 domain widening_delay
      (state2, inv1 @ inv2)
  | Assignment (str, aexpr) ->
      let res = domain.eval_aexpr aexpr state 
      let new_state = Map.add str res state
      
      (domain.check_emptiness new_state, [])
  | Conditional (cond, expr1, expr2) ->
      let true_cond = domain.eval_bexpr cond state 
      let false_cond = domain.eval_bexpr (BUnOp ("!", cond)) state 
      let t, inv_t = if Map.isEmpty true_cond then (Map.empty, []) else eval expr1 true_cond domain widening_delay
      let f, inv_f = if Map.isEmpty false_cond then (Map.empty, []) else eval expr2 false_cond domain widening_delay
      
      (domain.union t f, inv_t @ inv_f)
      
  | IncDec (e, op) -> 
      let value = domain.find e state 
      let new_state = Map.add e (domain.eval_incdec value op) state

      (new_state, [])
     
  | While (bexpr, expr) -> 
      (* B[-b](lfp(fun x -> s V (D[S]oB[b])x )) *)
      let mutable fixpoint = false
      let mutable curr = state 
      let mutable succ = Map.empty
      let mutable un : Map<string, 'T> = Map.empty
      let mutable it : int = 1
      let mutable wide : Map<string, 'T> = Map.empty

      while not fixpoint do 
        let b = domain.eval_bexpr bexpr curr
        let eval_expr, _ = eval expr b domain it
        succ <- eval_expr
        un <- domain.union curr succ
        printfn "%i" it

        if it > widening_delay
            then wide <- domain.var_wise_widening1 curr un
                 if wide = curr 
                    then fixpoint <- true
                    else curr <- wide
            else if un = curr 
                    then fixpoint <- true 
                    else curr <- un
        it <- it + 1
        
      let nar = domain.var_wise_narrowing curr succ
      let exit_cond = domain.eval_bexpr (BUnOp("!", bexpr)) nar 

      (exit_cond, [Some nar])
