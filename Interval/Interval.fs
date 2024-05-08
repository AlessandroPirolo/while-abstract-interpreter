module Interval

open Number

type Interval = 
    | Interval of Number * Number 
    | Empty 
    | Z
    with 
        (* Pointwise union *)
        static member lub x y =
            match (x, y) with
            | Interval (a, b), Interval (c, d) -> Interval (Number.min [a; c], Number.max [b; d])
            | _, _ -> Empty

        static member glb x y =
            match (x, y) with
            | Interval (a, b), Interval (c, d) ->
                let ma = Number.max [ a; c ] in
                let mi = Number.min [ b; d ] in
                if ma <= mi then Interval (ma, mi) else Empty
            | _, _ -> Empty

        static member ( ~- ) i =
            match i with Interval (a, b) -> Interval (-b, -a) | _ -> Empty

        static member ( + ) (x, y) =
            match (x, y) with
            | Interval (a, b), Interval (c, d) -> Interval (a + c, b + d)
            | _, _ -> Empty

        static member ( - ) (x, y) =
            match (x, y) with
            | Interval (a, b), Interval (c, d) -> Interval (a - d, b - c)
            | _, _ -> Empty

        static member ( * ) (x, y) =
            match (x, y) with
            | Interval (a, b), Interval (c, d) ->
                let ac = a * c 
                let ad = a * d 
                let bc = b * c 
                let bd = b * d 

                let mi = Number.min [ ac; ad; bc; bd ] in
                let ma = Number.max [ ac; ad; bc; bd ] in

                Interval (mi, ma)
            | _, _ -> Empty

        static member ( / ) (x, y) =
            match (x, y) with
            | Interval (a, b), Interval (c, d) ->
                let ac = a / c 
                let ad = a / d 
                let bc = b / c 
                let bd = b / d 

                if Number.Num 1 <= c then
                    let mi = Number.min [ ac; ad ] 
                    let ma = Number.max [ bc; bd ] 
                    Interval (mi, ma)
                else if d <= Num (-1) then
                    let ma = Number.max [ ac; ad ] 
                    let mi = Number.min [ bc; bd ] 
                    Interval (mi, ma)
                else
                    let y1 = Interval (Num 1, PlusInf) 
                    let y2 = Interval (MinInf, Num (-1)) 
                    Interval.lub (x / Interval.glb y y1) (x / Interval.glb y y2)
            | _, _ -> Empty

        static member (=.) (x, y) =
            match (x, y) with
            | Interval (a, b), Interval (c, d) -> 
                if a =. c && b =. d then true else false 
            | Empty, Empty 
            | Z, Z -> true
            | Empty, Z 
            | Z, Empty 
            | Interval (_,_), _ 
            | _, Interval (_,_) -> false 

        member this.AbstractInc = this + Interval (Num 1, Num 1)
        member this.AbstractDec = this - Interval (Num 1, Num 1)

        static member widening x y = 
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

        static member widening1 x y = 
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

        static member narrowing x y =
            match (x,y) with 
            | Interval (a, b), Interval(c, d) ->
                let f = if a =. Number.MinInf then c else a in
                let s = if b =. Number.PlusInf then d else b in
                Interval (f, s)
            | Interval (_,_), (Empty|Z) -> x
            | (Empty|Z), Interval (_,_) -> y
            | _, _ -> x

        member this.ToString = 
            match this with
            | Interval (a, b) -> "[" + a.ToString + ", " + b.ToString + "]" 
            | Empty -> "Ø"
            | Z -> "T"



