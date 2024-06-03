module AbstractDomain

open Ast

[<AbstractClass>]
type AbstractDomain<'T>() = 
    // abstract member
    abstract member default_value: 'T
    abstract member empty_val: 'T
    
    // abstract method
    abstract member lub: 'T -> 'T -> 'T
    abstract member glb: 'T -> 'T -> 'T
    abstract member toString: 'T -> string
    abstract member widening: 'T -> 'T -> 'T
    abstract member widening1: 'T -> 'T -> 'T
    abstract member narrowing: 'T -> 'T -> 'T
    abstract member eval_aexpr: Aexpr -> Map<string, 'T> -> 'T
    abstract member eval_bexpr: Bexpr -> Map<string, 'T> -> Map<string, 'T>
    abstract member eval_incdec: 'T -> string -> 'T

    // concrete method
    member this.find (k : string) (m : Map<string,'T>) : 'T = 
        match Map.tryFind k m with 
        | Some v -> v
        | None -> this.empty_val
    
    member this.exists (k : string) (m : Map<string, 'T>) : bool = Map.exists (fun k1 _ -> k1 = k) m

    member this.union (m1 : Map<string,'T>) (m2 : Map<string,'T>) : Map<string,'T> = 
        (m1, m2) ||> Map.fold (fun acc k v -> if this.exists k acc then Map.add k (this.lub (this.find k acc) v) acc else Map.add k v acc )

    member this.intersect (m1 : Map<string,'T>) (m2 : Map<string,'T>) : Map<string,'T> = 
        Map.fold (fun acc k v -> if this.exists k m2 then Map.add k v acc else acc) Map.empty m1

    member this.var_wise_widening s1 s2 = 
        Map.fold (fun acc k v -> if this.exists k acc then Map.add k (this.widening (this.find k acc) v) acc else acc) s1 s2
    
    member this.var_wise_widening1 s1 s2 = Map.fold (fun acc k v -> if this.exists k acc then Map.add k (this.widening1 (this.find k acc) v) acc else acc) s1 s2

    member this.var_wise_narrowing s1 s2 = Map.fold (fun acc k v -> Map.add k (this.narrowing (this.find k acc) v) acc) s1 s2

    member this.init_state (stmt : Statement) : Map<string,'T> =
        let rec find_var stmt = 
            match stmt with
            | Skip -> Set.empty
            | Composition (expr1, expr2) ->
                Set.union (find_var expr1) (find_var expr2)
            | Assignment (str, _) ->
                Set.singleton str
            | Conditional (_, expr1, expr2) ->
                Set.union (find_var expr1) (find_var expr2)
            | IncDec (e, _) -> 
                Set.singleton e
            | While (_, expr) -> 
                find_var expr

        find_var stmt
        |> Set.toList
        |> List.map( fun x -> (x,this.default_value))
        |> Map.ofList

    member this.stateToString (state : Map<string,'T>) : string = 
        "{" + match Map.isEmpty state with
                | false -> (" ", state) ||> Map.fold (fun acc k v -> acc + k + " -> " + this.toString v + "; " )
                | true -> "Ø" 
        + "}"

    

