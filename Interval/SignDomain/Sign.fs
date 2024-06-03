module Sign

type Sign =
    | Neg
    | Pos
    | Top
    | Bottom
    | Zero
    with 
        static member (+) (x, y) = 
            match x, y with
            | Neg, Neg
            | Zero, Neg
            | Neg, Zero -> Neg
            | Pos, Pos
            | Pos, Zero 
            | Zero, Pos -> Pos
            | Zero, Zero -> Zero
            | Bottom, _ 
            | _, Bottom -> Bottom
            | _, _ -> Top

        static member (-) (x, y) = 
            match x, y with
            | Neg, Neg -> Top 
            | Zero, Neg -> Pos
            | Neg, Zero -> Neg
            | Pos, Pos -> Top
            | Pos, Zero -> Pos
            | Zero, Pos -> Neg
            | Zero, Zero -> Zero
            | Bottom, _ 
            | _, Bottom -> Bottom
            | _, _ -> Top

        static member ( * ) (x, y) = 
            match x, y with
            | Zero, _ 
            | _, Zero -> Zero
            | Pos, Pos -> Pos
            | Pos, Neg -> Neg
            | Neg, Pos -> Neg
            | Neg, Neg -> Pos
            | _, Top -> Top
            | Top, _ -> Top
            | _ -> Bottom


        static member (/) (x, y) = 
            match x, y with
            | Zero, _ -> Zero
            | _, Zero -> Bottom
            | Pos, Pos -> Pos
            | Pos, Neg -> Neg
            | Neg, Pos -> Neg
            | Neg, Neg -> Pos
            | _, Top -> Top
            | Top, _ -> Top
            | _ -> Bottom

        static member ( ~- ) x = 
            match x with
            | Neg -> Pos
            | Zero -> Zero
            | Pos -> Neg
            | Bottom -> Bottom
            | Top -> Top

        static member lub x y =
            match (x, y) with
            | _, Bottom
            | Bottom, _ -> Bottom
            | Neg, Neg -> Neg
            | Zero, Zero -> Zero
            | Pos, Pos -> Pos
            | Pos, Zero
            | Zero, Pos -> Pos
            | Neg, Zero
            | Zero, Neg -> Neg
            | _ -> Top 

        static member glb x y =
            match x, y with
            | _, Bottom
            | Bottom, _ -> Bottom
            | Zero, _
            | _, Zero -> Zero
            | Top, _ -> y
            | _, Top -> x
            | Pos, Pos -> Pos
            | Neg, Neg -> Neg
            | Pos, Neg
            | Neg, Pos -> Zero

        static member (=.) (x, y) =
            match (x, y) with
            | Neg, Neg
            | Pos, Pos
            | Zero, Zero 
            | Top, Top
            | Bottom, Bottom -> true
            | _ -> false

        member this.AbstractInc =
            match this with 
            | Neg -> Top
            | Zero -> Pos
            | Pos -> Pos
            | Bottom -> Bottom
            | Top -> Top

        member this.AbstractDec = 
            match this with 
            | Neg -> Neg
            | Zero -> Neg
            | Pos -> Top
            | Bottom -> Bottom
            | Top -> Top