module IntervalDomain

open Interval
open Number
open Ast
open AbstractState

type IntervalDomain(lb : Number, ub : Number) = 
    new() = IntervalDomain(MinInf, PlusInf)

    member this.check_interval (int : Interval) : Interval = 
        match int with
        | Interval(a, b) -> 
            let a = if lb >. a then MinInf else a
            let b = if b >. ub then PlusInf else b
            Interval(a, b)
        | _ -> int

    member this.eval_incdec (v : Interval) (op : string) : Interval =
        match op with
        | "++" -> this.check_interval (v.AbstractInc)
        | "--" -> this.check_interval (v.AbstractDec)
        | _ -> v    

    member this.eval_aexpr (expr : Aexpr) (state : State) : Interval =
        let res = match expr with
                    | AConst n -> Interval (Num n, Num n)
                    | Var s -> find s state
                    | Neg s -> - (this.eval_aexpr s state)
                    | ABinOp (op1, e, op2) -> 
                        let f = this.eval_aexpr op1 state
                        let s = this.eval_aexpr op2 state
                        match e with
                        | "+" -> f + s
                        | "-" -> f - s
                        | "*" -> f * s
                        | "/" -> f / s
                        | _ -> raise (Error "Unknown operation!")
        this.check_interval res

    member this.eval_bexpr (expr : Bexpr) (state : State) : State =
        match expr with
        | BConst true -> state
        | BConst false -> Map.empty
        | BUnOp (op, e) -> 
            match op with
            | "!" -> this.eval_neg e state
            | _ -> raise (Error "Unknown unary operator!")
        | BBinOp (e1, op, e2) -> 
            let res1 = this.eval_bexpr e1 state
            let res2 = this.eval_bexpr e2 state
            match op with
            | "&&" -> intersect res1 res2
            | "||" -> union res1 res2 
            | _ -> state
        | BoolRelation (e1, op, e2) -> this.eval_bool_rel e1 op e2 state

    member this.eval_neg (bexpr : Bexpr) =
        match bexpr with
        | BConst true -> this.eval_bexpr (BConst false)
        | BConst false -> this.eval_bexpr (BConst true)
        | BUnOp (op, e) -> 
            match op with
            | "!" -> this.eval_bexpr e
            | _ -> raise (Error "Unknown unary operator!")
        | BBinOp (e1, op, e2) -> 
            match op with
            | "&&" -> this.eval_bexpr (BBinOp (BUnOp("!", e1), "||", BUnOp("!", e2)))
            | "||" -> this.eval_bexpr (BBinOp (BUnOp("!", e1), "&&", BUnOp("!", e2)))
            | _ -> id
        | BoolRelation (e1, op, e2) -> 
            let nop = 
                match op with
                | "=" -> "!="
                | "!=" -> "="
                | "<" -> ">="
                | "<=" -> ">"
                | ">" -> "<="
                | ">=" -> "<"
                | _ -> " "
            this.eval_bexpr (BoolRelation (e1, nop, e2)) 

    member this.eval_bool_rel (aexpr1: Aexpr) (op: string) (aexpr2 : Aexpr) (state : State) : State = 
        match op with
        | "=" -> 
            match (aexpr1, aexpr2) with
            | AConst x, AConst y -> if x = y then state else Map.empty
            | Var x, Var y -> 
                let res1 = this.eval_aexpr aexpr1 state
                let res2 = this.eval_aexpr aexpr2 state
                match (res1, res2) with
                | Interval (a, b), Interval (c, d) ->
                    if a >. d then Map.empty
                    else if c >. b then Map.empty
                    else
                        let res = Interval.glb res1 res2
                        let s = add x res state
                        add y res s 
                | _ -> state
            | Var x, AConst c -> 
                let res = this.eval_aexpr aexpr1 state
                match res with
                | Interval (a, b) ->
                    let constInt = Interval(Num c, Num c)
                    if a >. Num c then Map.empty
                    else
                        if Num (c) >. b then Map.empty
                        else 
                            add x (Interval.glb res constInt) state
                | _ -> state
            | AConst c, Var x -> 
                let res = this.eval_aexpr aexpr2 state 
                match res with
                | Interval (a, b) ->
                    let constInt = Interval(Num c, Num c)
                    if a >. Num c then Map.empty
                    else if Num c >. b then Map.empty
                    else 
                        add x (Interval.glb res constInt) state
                | _ -> state
            | _ -> state
        | "!=" -> 
            match (aexpr1, aexpr2) with
            | AConst x, AConst y -> if x <> y then state else Map.empty
            | Var _, Var _ -> 
                let res1 = this.eval_aexpr aexpr1 state
                let res2 = this.eval_aexpr aexpr2 state 
                match (res1, res2) with
                | Interval (a, b), Interval (c, d) -> 
                    if a != c || b != d then state
                    else Map.empty
                | _ -> state
            | Var x, AConst c -> 
                let res = this.eval_aexpr aexpr1 state
                match res with 
                | Interval (a, b) ->
                    if Num c >. a && b >. Num c then 
                        Map.empty
                    else if a >. Num c || Num c >. b then state
                    else 
                        if Num c =. a && b >. Num c then 
                            add x (Interval((Num 1 + a), b)) state
                        else 
                            if Num c =. b && Num c >. a then 
                                add x (Interval(a, (b - Num 1))) state
                            else 
                                state
                | _ -> state
            | AConst _, Var _ -> 
                this.eval_bexpr (BoolRelation(aexpr2, "!=", aexpr1)) state
            | _ -> state
      
        | ">" -> 
            match (aexpr1, aexpr2) with
            | AConst x, AConst y -> if x > y then state else Map.empty
            | Var x, Var y -> 
                let res1 = this.eval_aexpr aexpr1 state 
                let res2 = this.eval_aexpr aexpr2 state
                match (res1, res2) with
                | Interval (a, b), Interval (c, d) ->
                    if b <=. c then Map.empty
                    else
                        let s = add x (Interval (Number.max [a; c + Num 1], b)) state // check
                        add y (Interval (c,Number.min [b - Num 1; d])) s  // Check
                | _ -> state
            | Var x, AConst c -> 
                let res = this.eval_aexpr aexpr1 state
                match res with
                | Interval (a, b) ->
                    if b <=. Num c then Map.empty
                    else add x (Interval (Number.max [a; Num c + Num 1], b)) state // check
                | _ -> state
            | AConst c, Var x -> 
                let res = this.eval_aexpr aexpr2 state 
                match res with
                | Interval (a, b) ->
                    if Num c <=. a then Map.empty
                    else add x (Interval (a, Number.min [b; Num c - Num 1])) state // check
                | _ -> state
            | _ -> state
        | ">=" -> this.eval_bexpr (BoolRelation (aexpr2, "<=", aexpr1)) state
        | "<" -> this.eval_bexpr (BoolRelation (aexpr2, ">", aexpr1)) state
        | "<=" -> 
            match (aexpr1, aexpr2) with
            | AConst x, AConst y -> if x <= y then state else Map.empty
            | Var x, Var y -> 
                let res1 = this.eval_aexpr aexpr1 state 
                let res2 = this.eval_aexpr aexpr2 state 
                match (res1, res2) with
                | Interval (a, b), Interval (c, d) ->
                    if a >. d then Map.empty
                    else
                        let s = add x (Interval (a, Number.min [ b; d ])) state 
                        add y (Interval (Number.max [ c; a ], d)) s
                | _ -> state
            | Var x, AConst c -> 
                let res = this.eval_aexpr aexpr1 state
                match res with
                | Interval (a, b) ->
                    if a >. Num c then Map.empty
                    else add x (Interval (a, Number.min [ b; Num c ])) state  
                | _ -> state
            | AConst c, Var x -> 
                let res = this.eval_aexpr aexpr2 state 
                match res with
                | Interval (a, b) ->
                    if Num c >. b then Map.empty
                    else add x (Interval (Number.max [ a; Num c ], b)) state
                | _ -> state
            | _ -> state
        | _ -> state


