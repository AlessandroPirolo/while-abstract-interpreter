module IntervalDomain

open Interval
open Number
open Ast
open AbstractDomain

type IntervalDomain(lb : Number, ub : Number) = 
    inherit AbstractDomain<Interval>()

    new() = IntervalDomain(MinInf, PlusInf)

    member private this.check_interval (int : Interval) : Interval = 
        match int with
        | Interval(a, b) when a != b -> 
            let a = if lb >. a then MinInf else a
            let b = if b >. ub then PlusInf else b
            Interval(a, b)
        | _ -> int

    // abstract member
    override this.default_value = Z
    override this.empty_val = Empty
    
    // abstract method
    override this.lub x y = this.check_interval (Interval.lub x y)

    override this.glb x y = this.check_interval (Interval.glb x y)

    override this.toString v : string = 
            match v with
            | Interval (a, b) -> "[" + a.ToString + ", " + b.ToString + "]" 
            | Empty -> "Empty"
            | Z -> "T"

    override this.widening x y = 
            match (x,y) with 
            | Interval (a, b), Interval(c, d) ->
                let f = 
                    if a <=. c then a 
                    else if Num 0 <=. c &&  a >. c then Num 0
                    else Number.MinInf 
                let s = 
                    if d <=. b then b 
                    else if b <=. Num 0 && b >. d then Num 0 
                    else Number.PlusInf 
                Interval (f, s)
            | Interval (_,_), (Empty|Z) -> x
            | (Empty|Z), Interval (_,_) -> y
            | _, _ -> x

    override this.widening1 x y = 
            match (x,y) with 
            | Interval (a, b), Interval(c, d) ->
                let f = 
                    if a <=. c then a 
                    else MinInf
                let s = 
                    if d <=. b then b 
                    else PlusInf
                Interval (f, s)
            | Interval (_,_), (Empty|Z) -> x
            | (Empty|Z), Interval (_,_) -> y
            | _, _ -> x
    
    override this.narrowing x y =
            match (x,y) with 
            | Interval (a, b), Interval(c, d) ->
                let f = if a =. Number.MinInf then c else a in
                let s = if b =. Number.PlusInf then d else b in
                Interval (f, s)
            | Interval (_,_), (Empty|Z) -> x
            | (Empty|Z), Interval (_,_) -> y
            | _, _ -> x

    override this.eval_incdec (v : Interval) (op : string) : Interval =
        match op with
        | "++" -> this.check_interval (v.AbstractInc)
        | "--" -> this.check_interval (v.AbstractDec)
        | _ -> v    

    override this.eval_aexpr (expr : Aexpr) (state : Map<string, Interval>) : Interval =
        let res = match expr with
                    | AConst n -> Interval (Num n, Num n)
                    | Var s -> this.find s state
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

    override this.eval_bexpr (expr : Bexpr) (state : Map<string, Interval>) : Map<string, Interval> =
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
            | "&&" -> this.intersect res1 res2
            | "||" -> this.union res1 res2 
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

    member this.eval_bool_rel (aexpr1: Aexpr) (op: string) (aexpr2 : Aexpr) (state : Map<string, Interval>) : Map<string, Interval> = 
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
                        let s = Map.add x res state
                        Map.add y res s 
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
                            Map.add x (Interval.glb res constInt) state
                | _ -> state
            | AConst c, Var x -> 
                let res = this.eval_aexpr aexpr2 state 
                match res with
                | Interval (a, b) ->
                    let constInt = Interval(Num c, Num c)
                    if a >. Num c then Map.empty
                    else if Num c >. b then Map.empty
                    else 
                        Map.add x (Interval.glb res constInt) state
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
                            Map.add x (Interval((Num 1 + a), b)) state
                        else 
                            if Num c =. b && Num c >. a then 
                                Map.add x (Interval(a, (b - Num 1))) state
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
                        let s = Map.add x (Interval (Number.max [a; c + Num 1], b)) state // check
                        Map.add y (Interval (c,Number.min [b - Num 1; d])) s  // Check
                | _ -> state
            | Var x, AConst c -> 
                let res = this.eval_aexpr aexpr1 state
                match res with
                | Interval (a, b) ->
                    if b <=. Num c then Map.empty
                    else Map.add x (Interval (Number.max [a; Num c + Num 1], b)) state // check
                | _ -> state
            | AConst c, Var x -> 
                let res = this.eval_aexpr aexpr2 state 
                match res with
                | Interval (a, b) ->
                    if Num c <=. a then Map.empty
                    else Map.add x (Interval (a, Number.min [b; Num c - Num 1])) state // check
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
                        let s = Map.add x (Interval (a, Number.min [ b; d ])) state 
                        Map.add y (Interval (Number.max [ c; a ], d)) s
                | _ -> state
            | Var x, AConst c -> 
                let res = this.eval_aexpr aexpr1 state
                match res with
                | Interval (a, b) ->
                    if a >. Num c then Map.empty
                    else Map.add x (Interval (a, Number.min [ b; Num c ])) state  
                | _ -> state
            | AConst c, Var x -> 
                let res = this.eval_aexpr aexpr2 state 
                match res with
                | Interval (a, b) ->
                    if Num c >. b then Map.empty
                    else Map.add x (Interval (Number.max [ a; Num c ], b)) state
                | _ -> state
            | _ -> state
        | _ -> state


