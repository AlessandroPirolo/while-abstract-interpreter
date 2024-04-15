module Interval

open Number

type Interval = 
    | Interval of Number * Number 
    | Empty 
    | Z
    with 
        (* Operations on interval *)
        static member lub x y =
            match (x, y) with
            | Interval (a, b), Interval (c, d) -> Interval (min a c, max b d)
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

        member this.AbstractInc = this + Interval (Num 1, Num 1)
        member this.AbstractDec = this - Interval (Num 1, Num 1)

        static member widening x y = 
            match (x,y) with 
            | Interval (a, b), Interval(c, d) ->
                let f = if a <= c then a else Number.MinInf in
                let s = if b>=d then b else Number.PlusInf in
                Interval (f, s)
            | Interval (_,_), (Empty|Z) -> x
            | (Empty|Z), Interval (_,_) -> y
            | _, _ -> x
end