open Number

let ( /- ) = Number.( - )
let ( // ) = Number.( / )
let ( /+ ) = Number.( + )
let ( /* ) = Number.( * )
let ( ! ) = Number.( !- )

module Interval = struct
  type interval = Interval of Number.number * Number.number | Empty | Z

  (* Operations on interval *)
  let lub x y =
    match (x, y) with
    | Interval (a, b), Interval (c, d) -> Interval (min a c, max b d)
    | _, _ -> Empty

  let glb x y =
    match (x, y) with
    | Interval (a, b), Interval (c, d) ->
        let ma = Number.max [ a; c ] in
        let mi = Number.min [ b; d ] in
        if ma <= mi then Interval (ma, mi) else Empty
    | _, _ -> Empty

  let ( !- ) i =
    match i with Interval (a, b) -> Interval (!b, !a) | _ -> Empty

  let ( + ) x y =
    match (x, y) with
    | Interval (a, b), Interval (c, d) -> Interval (a /+ c, b /+ d)
    | _, _ -> Empty

  let ( - ) x y =
    match (x, y) with
    | Interval (a, b), Interval (c, d) -> Interval (a /- d, b /- c)
    | _, _ -> Empty

  let ( * ) x y =
    match (x, y) with
    | Interval (a, b), Interval (c, d) ->
        let ac = a /* c in
        let ad = a /* d in
        let bc = b /* c in
        let bd = b /* d in

        let mi = Number.min [ ac; ad; bc; bd ] in
        let ma = Number.max [ ac; ad; bc; bd ] in

        Interval (mi, ma)
    | _, _ -> Empty

  let rec ( / ) x y =
    match (x, y) with
    | Interval (a, b), Interval (c, d) ->
        let ac = a // c in
        let ad = a // d in
        let bc = b // c in
        let bd = b // d in

        if Number.Num 1 <= c then
          let mi = Number.min [ ac; ad ] in
          let ma = Number.max [ bc; bd ] in
          Interval (mi, ma)
        else if d <= Number.Num (-1) then
          let ma = Number.max [ ac; ad ] in
          let mi = Number.min [ bc; bd ] in
          Interval (mi, ma)
        else
          let y1 = Interval (Number.Num 1, Number.PlusInf) in
          let y2 = Interval (Number.MinInf, Number.Num (-1)) in
          lub (x / glb y y1) (x / glb y y2)
    | _, _ -> Empty

  let abstract_inc x = x + Interval (Num 1, Num 1)
  let abstract_dec x = x - Interval (Num 1, Num 1)
end
