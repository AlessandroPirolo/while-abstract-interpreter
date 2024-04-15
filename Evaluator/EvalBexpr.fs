module EvalBexpr

open AbstractState
open Ast
open Interval
open Number
open EvalAexpr

(* Conditional evaluation *)
let rec eval_bexpr (expr : Bexpr) (state : State) : State =
  match expr with
  | BConst true -> state
  | BConst false -> Map.empty
  | BUnOp (op, e) -> 
      match op with
      | "!" -> eval_neg e state
      | _ -> raise (Error "Unknown unary operator!")
  | BBinOp (e1, op, e2) -> 
      let res1 = eval_bexpr e1 state
      let res2 = eval_bexpr e2 state
      match op with
      | "&&" -> intersect res1 res2
      | "||" -> union res1 res2 
      | _ -> state
  | BoolRelation (e1, op, e2) -> eval_bool_rel e1 op e2 state

and eval_neg (bexpr : Bexpr) =
    match bexpr with
    | BConst true -> eval_bexpr (BConst false)
    | BConst false -> eval_bexpr (BConst true)
    | BUnOp (op, e) -> 
        match op with
        | "!" -> eval_bexpr e
        | _ -> raise (Error "Unknown unary operator!")
    | BBinOp (e1, op, e2) -> 
        match op with
        | "&&" -> eval_bexpr (BBinOp (BUnOp("!", e1), "||", BUnOp("!", e2)))
        | "||" -> eval_bexpr (BBinOp (BUnOp("!", e1), "&&", BUnOp("!", e2)))
        | _ -> id
    | BoolRelation (e1, op, e2) -> 
        let nop = 
            match op with
            | "=" -> "!="
            | "!=" -> "="
            | "<" -> ">"
            | "<=" -> ">="
            | ">" -> "<"
            | ">=" -> "<="
            | _ -> " "
        eval_bexpr (BoolRelation (e1, nop, e2)) 
 
and eval_bool_rel (aexpr1: Aexpr) (op: string) (aexpr2 : Aexpr) (state : State) : State = 
    match op with
    | "=" -> 
        match (aexpr1, aexpr2) with
          | AConst x, AConst y -> if x = y then state else Map.empty
          | Var x, Var y -> 
              let res1 = eval_aexpr aexpr1 state
              let res2 = eval_aexpr aexpr2 state
              match (res1, res2) with
              | Interval (a, b), Interval (c, d) ->
                  if d < a then Map.empty
                  else if c > b then Map.empty
                  else
                    let res = Interval.glb res1 res2
                    let s = add x res state
                    add y res s 
              | _ -> state
          | Var x, AConst c -> 
              let res = eval_aexpr aexpr1 state
              match res with
              | Interval (a, b) ->
                  let constInt = Interval(Num c, Num c)
                  if a > Num c then Map.empty
                  else
                    if Num (c) > b then Map.empty
                    else 
                      add x (Interval.glb res constInt) state
              | _ -> state
          | AConst c, Var x -> 
              let res = eval_aexpr aexpr2 state 
              match res with
              | Interval (a, b) ->
                  let constInt = Interval(Num c, Num c)
                  if a > Num c then Map.empty
                  else if Num (c) > b then Map.empty
                  else 
                      add x (Interval.glb res constInt) state
              | _ -> state
            | _ -> state
      | "!=" -> 
          match (aexpr1, aexpr2) with
          | AConst x, AConst y -> if x <> y then state else Map.empty
          | Var _, Var _ -> 
              let res1 = eval_aexpr aexpr1 state
              let res2 = eval_aexpr aexpr2 state 
              match (res1, res2) with
              | Interval (a, b), Interval (c, d) -> 
                  if a != c || b != d then state
                  else Map.empty
              | _ -> state
          | Var x, AConst c -> 
              let res = eval_aexpr aexpr1 state
              match res with 
              | Interval (a, b) ->
                  if a < Num c && Num c < b then 
                    Map.empty
                  else if Num c < a || Num c >. b then state
                  else 
                      if Num c = a && Num c < b then 
                        add x (Interval((Num 1 + a), b)) state
                      else 
                        if Num c = b && Num c >. a then 
                          add x (Interval(a, (b - Num 1))) state
                        else 
                          state
              | _ -> state
          | AConst _, Var _ -> 
              eval_bexpr (BoolRelation(aexpr2, "!=", aexpr1)) state
          | _ -> state
      
      | ">" -> 
          match (aexpr1, aexpr2) with
          | AConst x, AConst y -> if x > y then state else Map.empty
          | Var x, Var y -> 
              let res1 = eval_aexpr aexpr1 state 
              let res2 = eval_aexpr aexpr2 state
              match (res1, res2) with
              | Interval (a, b), Interval (c, d) ->
                  if b <=. c then Map.empty
                  else
                    let s = add x (Interval (Number.max [a; c], b)) state in
                    add y (Interval (c,Number.min [b; d])) s
              | _ -> state
          | Var x, AConst c -> 
              let res = eval_aexpr aexpr1 state
              match res with
              | Interval (a, b) ->
                  if b <=. Num c then Map.empty
                  else add x (Interval (Number.max [a; Num c], b)) state
              | _ -> state
          | AConst c, Var x -> 
              let res = eval_aexpr aexpr2 state 
              match res with
              | Interval (a, b) ->
                  if Num c <=. a then Map.empty
                  else add x (Interval (a, Number.min [b; Num c])) state
              | _ -> state
          | _ -> state
      | ">=" -> eval_bexpr (BoolRelation (aexpr2, "<=", aexpr1)) state
      | "<" -> eval_bexpr (BoolRelation (aexpr2, ">", aexpr1)) state
      | "<=" -> 
          match (aexpr1, aexpr2) with
          | AConst x, AConst y -> if x <= y then state else Map.empty
          | Var x, Var y -> 
              let res1 = eval_aexpr aexpr1 state 
              let res2 = eval_aexpr aexpr2 state 
              match (res1, res2) with
              | Interval (a, b), Interval (c, d) ->
                  if a >. d then Map.empty
                  else
                    let s = add x (Interval (a, Number.min [ b; d ])) state 
                    add y (Interval (Number.max [ c; a ], d)) s
              | _ -> state
          | Var x, AConst c -> 
              let res = eval_aexpr aexpr1 state
              match res with
              | Interval (a, b) ->
                  if a >. Num c then Map.empty
                  else add x (Interval (a, Number.min [ b; Num c ])) state
              | _ -> state
          | AConst c, Var x -> 
              let res = eval_aexpr aexpr2 state 
              match res with
              | Interval (a, b) ->
                  if Num c >. b then Map.empty
                  else add x (Interval (Number.max [ a; Num c ], b)) state
              | _ -> state
          | _ -> state
      | _ -> state



