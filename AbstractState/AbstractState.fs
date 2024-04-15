module AbstractState

open Interval
open Ast

type State = Map<string,Interval>
 
let add (k : string) (v : Interval) (m : State) : State = Map.add k v m
let find (k : string) m = Map.find k m
let isEmpty m = Map.isEmpty m
let exists (k : string) (m : State) = Map.exists (fun k1 _ -> k1 = k) m
let union m1 m2 = Map.fold (Interval.lub ) m1 m2
let intersect m1 m2 = Map.fold (fun acc k v -> if exists k m2 then add k v acc else acc) Map.empty m1

let init_state (stmt : Statement) : State =
    let rec find_var stmt = 
        match stmt with
        | Skip -> Set.empty
        | Composition (expr1, expr2) ->
            Set.union (find_var expr1) (find_var expr2)
        | Assignment (str, _) ->
            Set.singleton str
        | Conditional (_, expr1, expr2) ->
            Set.union (find_var expr1) (find_var expr2)
        | IncDec (_, e) -> 
            Set.singleton e
        | While (_, expr) -> 
            find_var expr

    find_var stmt
    |> Set.toList
    |> List.map( fun x -> (x,Z))
    |> Map.ofList

let to_string (state : State) : string = 
    (" ", state) ||> Map.fold (fun acc k v -> acc + k + " := " + v.ToString + "; " )
    