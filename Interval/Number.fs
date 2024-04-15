module Number

exception Error of string

type Number = 
    | MinInf 
    | PlusInf 
    | Num of int
    (* Operations on numbers *)
with
        static member ( + ) (n1,n2) =
            match (n1, n2) with
            | MinInf, MinInf 
            | Num _, MinInf 
            | MinInf, Num _ -> MinInf
            | Num n1, Num n2 -> Num (n1 + n2)
            | PlusInf, PlusInf
            | Num _, PlusInf 
            | PlusInf, Num _ 
            | MinInf, PlusInf 
            | PlusInf, MinInf -> PlusInf
    
        static member ( - ) (n1, n2) =
            match (n1, n2) with
            | Num _, MinInf 
            | MinInf, Num _ 
            | MinInf, MinInf -> MinInf
            | Num n1, Num n2 -> Num (n1 - n2)
            | PlusInf, PlusInf 
            | Num _, PlusInf 
            | PlusInf, Num _
            | MinInf, PlusInf 
            | PlusInf, MinInf -> PlusInf

        static member ( ~- ) n =
            match n with 
            | Num n -> Num (-1 * n) 
            | PlusInf -> MinInf 
            | MinInf -> PlusInf

        static member ( * ) (n1, n2) =
            match (n1, n2) with
            | Num n1, Num n2 -> Num (n1 * n2)
            | PlusInf, Num _ 
            | Num _, PlusInf 
            | PlusInf, PlusInf 
            | MinInf, MinInf -> PlusInf
            | Num 0, _ | _, Num 0 -> Num 0
            | PlusInf, MinInf 
            | MinInf, PlusInf 
            | MinInf, Num _ 
            | Num _, MinInf -> MinInf

        static member ( / ) (n1, n2) =
            match (n1, n2) with
            | Num 0, _ -> Num 0
            | _, Num 0 -> raise (Error "Division by 0 not allowed!")
            | Num n1, Num n2 -> Num (n1 / n2)
            | PlusInf, MinInf 
            | MinInf, PlusInf
            | MinInf, Num _ -> MinInf
            | PlusInf, Num _ 
            | PlusInf, PlusInf 
            | MinInf, MinInf -> PlusInf
            | Num _, PlusInf -> Num 0
            | Num _, MinInf -> Num 0

        static member ( <=. ) (n1, n2) =
            match (n1, n2) with
            | Num n1, Num n2 -> n1 <= n2
            | MinInf, PlusInf -> true
            | MinInf, MinInf -> true
            | MinInf, Num _ -> true
            | Num _, MinInf -> false
            | PlusInf, MinInf -> false
            | PlusInf, Num _ -> false
            | Num _, PlusInf -> true
            | PlusInf, PlusInf -> true

        static member ( >. ) (n1, n2) =
            match (n1, n2) with
            | Num n1, Num n2 -> n1 > n2
            | MinInf, PlusInf 
            | Num _, PlusInf 
            | MinInf, Num _ -> false
            | Num _, MinInf 
            | PlusInf, MinInf 
            | PlusInf, Num _ 
            | MinInf, MinInf 
            | PlusInf, PlusInf -> true

        static member ( != ) (n1, n2) =
            match (n1, n2) with
            | Num n1, Num n2 -> n1 <> n2
            | MinInf, PlusInf 
            | MinInf, Num _ 
            | Num _, MinInf 
            | PlusInf, MinInf 
            | PlusInf, Num _ 
            | Num _, PlusInf -> false
            | MinInf, MinInf 
            | PlusInf, PlusInf -> true
 
        static member min l =
            let rec find_min l m =
                match l with
                | [] -> m
                | h :: t -> if h <= m then find_min t h else find_min t m
            let m = List.head l
            find_min l m

        static member max l =
            let rec find_max l m =
                match l with
                | [] -> m
                | h :: t -> if h <= m then find_max t m else find_max t h
            let m = List.head l
            find_max l m
