open Interval
module StringMap = Map.Make (String)

module AbstractState = struct
  type state = Interval.interval StringMap.t

  let add k v (map : state) : state = StringMap.add k v map
  let find (k : string) (map : state) = StringMap.find k map
  let emptyState : Interval.interval StringMap.t = StringMap.empty
  let is_empty map = StringMap.is_empty map
  let exists k m = StringMap.exists (fun k1 _ -> k1 == k) m
  let union m1 m2 = StringMap.union (fun _ x _ -> Some x) m1 m2

  let intersect m1 m2 =
    StringMap.fold
      (fun k v acc -> if exists k m2 then add k v acc else acc)
      m1 emptyState
end
