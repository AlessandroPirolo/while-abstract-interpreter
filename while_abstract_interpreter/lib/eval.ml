open Ast
open Interval
open Abstract_state.AbstractState
open Number

exception Error of string

let ( + ) = Interval.( + )
let ( - ) = Interval.( - )
let ( * ) = Interval.( * )
let ( / ) = Interval.( / )
let (+.) = Number.(+)
let (-.) = Number.(-)


let rec eval_aexpr (expr : Ast.aexpr) (state : state) : Interval.interval =
  match expr with
  | AConst n -> Interval (Num n, Num n)
  | Var s -> find s state
  | Neg s -> Interval.( !- ) (find s state)
  | ABinOp (op1, e, op2) -> (
      let f = eval_aexpr op1 state in
      let s = eval_aexpr op2 state in
      match e with
      | "+" -> f + s
      | "-" -> f - s
      | "*" -> f * s
      | "/" -> f / s
      | _ -> raise (Error "Unknown operation!"))

(* Conditional evaluation *)
let rec eval_bexpr (expr : Ast.bexpr) (state : state) : state =
  match expr with
  | BConst true -> state
  | BConst false -> emptyState
  | BUnOp (op, e) -> (
      match op with
      | "!" -> (
          let b = eval_bexpr e state in
          match is_empty b with true -> state | false -> emptyState)
      | _ -> raise (Error "Unknown unary operator!"))
  | BBinOp (e1, op, e2) -> (
      let res1 = eval_bexpr e1 state in
      let res2 = eval_bexpr e2 state in
      match op with
      | "&&" -> intersect res1 res2
      | "||" -> union res1 res2
      | _ -> state)
  | BoolRelation (e1, op, e2) -> (
      match op with
      | "=" -> (
          match (e1, e2) with
          | AConst x, AConst y -> if x == y then state else emptyState
          | Var x, Var y -> (
              let res1 = eval_aexpr e1 state in
              let res2 = eval_aexpr e2 state in
              match (res1, res2) with
              | Interval (a, b), Interval (c, d) ->
                  if d < a then emptyState
                  else 
                    (if c > b then emptyState
                    else
                      let res = Interval.glb res1 res2 in 
                      let s = add x res state in
                      add y res s )
              | _ -> state)
          | Var x, AConst c -> (
              let res = eval_aexpr e1 state in
              match res with
              | Interval (a, b) ->
                  let constInt = Interval.Interval(Num c, Num c) in
                  if a > Num c then emptyState
                  else (
                    if Number.Num (c) > b then emptyState
                    else 
                      add x (Interval.glb res constInt) state
              )
              | _ -> state)
          | AConst c, Var x -> (
              let res = eval_aexpr e2 state in
              match res with
              | Interval (a, b) ->
                  let constInt = Interval.Interval(Num c, Num c) in
                  if a > Num c then emptyState
                  else (
                    if Number.Num (c) > b then emptyState
                    else 
                      add x (Interval.glb res constInt) state
              )
              | _ -> state)
          | _ -> state)
      | "!=" -> (
          match (e1, e2) with
          | AConst x, AConst y -> if x != y then state else emptyState
          | Var _, Var _ -> (
              let res1 = eval_aexpr e1 state in
              let res2 = eval_aexpr e2 state in
              match (res1, res2) with
              | Interval (a, b), Interval (c, d) -> 
                  if a != c || b != d then state
                  else emptyState
              | _ -> state)
          | Var x, AConst c -> (
              let res = eval_aexpr e1 state in
              match res with 
              | Interval (a, b) ->
                  if a < Number.Num c && Number.Num c < b then 
                    emptyState
                  else (
                    if Number.Num c < a || Number.Num c > b then 
                      state
                    else (
                      if Number.Num c = a && Number.Num c < b then 
                        add x (Interval.Interval((Number.Num 1 +. a), b)) state
                      else (
                        if Number.Num c = b && Number.Num c > a then 
                          add x (Interval.Interval(a, (b -. Number.Num 1))) state
                        else 
                          state
                      )
              )
              )
              | _ -> state)
          | AConst _, Var _ -> 
              eval_bexpr (BoolRelation(e2, "!=", e1)) state
          | _ -> state)
      
      | ">" -> (
          match (e1, e2) with
          | AConst x, AConst y -> if x > y then state else emptyState
          | Var x, Var y -> (
              let res1 = eval_aexpr e1 state in
              let res2 = eval_aexpr e2 state in
              match (res1, res2) with
              | Interval (a, b), Interval (c, d) ->
                  if b <= c then emptyState
                  else
                    let s = add x (Interval (Number.max [a; c], b)) state in
                    add y (Interval (c,Number.min [b; d])) s
              | _ -> state)
          | Var x, AConst c -> (
              let res = eval_aexpr e1 state in
              match res with
              | Interval (a, b) ->
                  if Number.Num c >= b then emptyState
                  else add x (Interval (Number.max [a; Num c], b)) state
              | _ -> state)
          | AConst c, Var x -> (
              let res = eval_aexpr e2 state in
              match res with
              | Interval (a, b) ->
                  if a >= Num c then emptyState
                  else add x (Interval (a, Number.min [b; Num c])) state
              | _ -> state)
          | _ -> state)
      | ">=" -> eval_bexpr (BoolRelation (e2, "<=", e1)) state
      | "<" -> eval_bexpr (BoolRelation (e2, ">", e1)) state
      | "<=" -> (
          match (e1, e2) with
          | AConst x, AConst y -> if x <= y then state else emptyState
          | Var x, Var y -> (
              let res1 = eval_aexpr e1 state in
              let res2 = eval_aexpr e2 state in
              match (res1, res2) with
              | Interval (a, b), Interval (c, d) ->
                  if a > d then emptyState
                  else
                    let s = add x (Interval (a, Number.min [ b; d ])) state in
                    add y (Interval (Number.max [ c; a ], d)) s
              | _ -> state)
          | Var x, AConst c -> (
              let res = eval_aexpr e1 state in
              match res with
              | Interval (a, b) ->
                  if a > Num c then emptyState
                  else add x (Interval (a, Number.min [ b; Num c ])) state
              | _ -> state)
          | AConst c, Var x -> (
              let res = eval_aexpr e2 state in
              match res with
              | Interval (a, b) ->
                  if b < Num c then emptyState
                  else add x (Interval (Number.max [ a; Num c ], b)) state
              | _ -> state)
          | _ -> state)
      | _ -> state)

let rec eval (stmt : Ast.statement) (state : state) (states : state list) :
    state =
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

      union t f 
  | IncDec (op, e) -> (
      let value = find e state in
      match op with
      | "++" -> add e (Interval.abstract_inc value) state
      | "--" -> add e (Interval.abstract_dec value) state
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
        let wide = Interval.widening prev.contents un in
        if wide = succ.contents then
          fixpoint := true
        else
          prev := succ.contents
      done;*)
      state
