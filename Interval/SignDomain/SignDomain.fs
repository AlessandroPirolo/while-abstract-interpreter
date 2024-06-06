module SignDomain

open AbstractDomain
open Sign
open Ast

type SignDomain() = 
    inherit AbstractDomain<Sign>()

    // abstract member
    override this.default_value = Top
    override this.empty_val = Bottom
    
    // abstract method
    override this.lub x y = Sign.lub x y

    override this.glb x y = Sign.glb x y

    override this.toString v : string = 
            match v with
            | Pos -> "+"
            | Sign.Neg -> "-"
            | Top -> "T"
            | Bottom -> "Empty"
            | Zero -> "0"

    override this.widening x y = y

    override this.widening1 x y = y
    
    override this.narrowing x y =
        match x, y with
        | Pos, Pos -> Pos
        | Sign.Neg, Sign.Neg -> Sign.Neg
        | Zero, Zero -> Zero
        | _, Bottom
        | Bottom, _ -> Bottom
        | _ -> Top


    override this.eval_incdec (v : Sign) (op : string) : Sign =
        match op with
        | "++" -> v.AbstractInc
        | "--" -> v.AbstractDec
        | _ -> v    

    override this.eval_aexpr (expr : Aexpr) (state : Map<string, Sign>) : Sign =
        let res = match expr with
                    | AConst n -> 
                        if n = 0 then
                            Zero
                        else
                            if n >= 0 then
                                Pos
                            else
                                Sign.Neg
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
                        | _ -> Bottom
        res

    override this.eval_bexpr (expr : Bexpr) (state : Map<string, Sign>) : Map<string, Sign> =
        let res = match expr with
                    | BConst true -> state
                    | BConst false -> Map.empty
                    | BUnOp (op, e) -> 
                        match op with
                        | "!" -> this.eval_neg e state
                        | _ -> Map.empty
                    | BBinOp (e1, op, e2) -> 
                        let res1 = this.eval_bexpr e1 state
                        let res2 = this.eval_bexpr e2 state
                        match op with
                        | "&&" -> this.intersect res1 res2
                        | "||" -> this.union res1 res2 
                        | _ -> state
                    | BoolRelation (e1, op, e2) -> this.eval_bool_rel e1 op e2 state
        this.check_emptiness res

    member this.eval_neg (bexpr : Bexpr) =
        match bexpr with
        | BConst true -> this.eval_bexpr (BConst false)
        | BConst false -> this.eval_bexpr (BConst true)
        | BUnOp (op, e) -> 
            match op with
            | "!" -> this.eval_bexpr e
            | _ -> fun s -> Map.empty
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

    member this.eval_bool_rel (aexpr1: Aexpr) (op: string) (aexpr2 : Aexpr) (state : Map<string, Sign>) : Map<string, Sign> = 
        match op with
        | "=" -> 
            match (aexpr1, aexpr2) with
            | AConst x, AConst y -> if x = y then state else Map.empty
            | _, _ -> 
                let res1 = this.eval_aexpr aexpr1 state
                let res2 = this.eval_aexpr aexpr2 state
                if res1 =. res2 then 
                    state
                else Map.empty
                
        | "!=" -> 
            match (aexpr1, aexpr2) with
            | AConst x, AConst y -> if x <> y then state else Map.empty
            | _, _ -> 
                let res1 = this.eval_aexpr aexpr1 state
                let res2 = this.eval_aexpr aexpr2 state 
                if not (res1 =. res2) then 
                    state
                else Map.empty
          
      
        | ">" -> this.eval_bexpr (BoolRelation (ABinOp (aexpr2, "+", AConst 1), "<=", aexpr1)) state
        | ">=" -> this.eval_bexpr (BoolRelation (aexpr2, "<=", aexpr1)) state
        | "<" -> this.eval_bexpr (BoolRelation (aexpr2, ">", aexpr1)) state
        | "<=" -> 
            match (aexpr1, aexpr2) with
            | AConst x, AConst y -> if x <= y then state else Map.empty
            | Var _, Var _ 
            | ABinOp _, ABinOp _
            | Neg _, ABinOp _ 
            | ABinOp _, Neg _ 
            | Neg _, Neg _
            | Neg _, Var _
            | Var _, Neg _ 
            | ABinOp _ , Var _
            | Var _, ABinOp _ -> 
                let res1 = this.eval_aexpr aexpr1 state 
                let res2 = this.eval_aexpr aexpr2 state 
                let x = 
                    match res2 with
                    | Zero
                    | Sign.Neg -> this.eval_bexpr (BoolRelation (aexpr1, "<=", AConst 0)) state
                    | Bottom -> Map.empty
                    | _ -> state
                let y = 
                    match res1 with
                    | Zero
                    | Pos -> this.eval_bexpr (BoolRelation (aexpr2, ">=", AConst 0)) state
                    | Bottom -> Map.empty
                    | _ -> state
                this.union x y
            | Var x, AConst c -> 
                let res = this.eval_aexpr aexpr1 state
                match res with
                | Pos
                | Zero ->
                    if c <= 0 then Map.add x Zero state
                    else state 
                | Top
                | Sign.Neg ->
                    if c <= 0 then Map.add x Sign.Neg state
                    else state 
                | Bottom -> Map.empty
            | AConst c, Var x -> 
                this.eval_bexpr (BoolRelation (Ast.Neg aexpr2, "<=", Ast.Neg aexpr1)) state
            
            | (ABinOp _ as r), AConst c
            | AConst c, (ABinOp _ as r) 
            | (Neg _ as r), AConst c
            | AConst c, (Neg _ as r) ->
                let res = this.eval_aexpr r state
                match res with
                | Pos
                | Zero ->
                    if c <= 0 then state else Map.empty
                | Top
                | Sign.Neg ->
                    if c <= 0 then state else Map.empty
                | Bottom -> Map.empty
            
        | _ -> state
