module AbstractState

open Interval

type State = Map<string,Interval>
 
let add (k : string) (v : Interval) (m : State) : State = Map.add k v m
let find (k : string) m = Map.find k m
let isEmpty m = Map.isEmpty m
let exists (k : string) (m : State) = Map.exists (fun k1 _ -> k1 = k) m
let union m1 m2 = Map.fold (fun acc k v -> add k v acc) m1 m2
let intersect m1 m2 = Map.fold (fun acc v k -> if exists k m2 then add k v acc else acc) m1 Map.empty

