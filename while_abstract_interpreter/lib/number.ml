module Number = struct
  exception Error of string

  type number = MinInf | PlusInf | Num of int

  (* Operations on numbers *)

  let ( + ) n1 n2 =
    match (n1, n2) with
    | Num _, MinInf | MinInf, Num _ -> MinInf
    | Num _, PlusInf | PlusInf, Num _ -> PlusInf
    | Num n1, Num n2 -> Num (n1 + n2)
    | PlusInf, PlusInf -> PlusInf
    | MinInf, MinInf -> MinInf
    | MinInf, PlusInf | PlusInf, MinInf -> PlusInf

  let ( - ) n1 n2 =
    match (n1, n2) with
    | Num _, MinInf | MinInf, Num _ -> MinInf
    | Num _, PlusInf | PlusInf, Num _ -> PlusInf
    | Num n1, Num n2 -> Num (n1 - n2)
    | PlusInf, PlusInf -> PlusInf
    | MinInf, MinInf -> MinInf
    | MinInf, PlusInf | PlusInf, MinInf -> PlusInf

  let ( !- ) n =
    match n with Num n -> Num (-1 * n) | PlusInf -> MinInf | MinInf -> PlusInf

  let ( * ) n1 n2 =
    match (n1, n2) with
    | Num n1, Num n2 -> Num (n1 * n2)
    | PlusInf, MinInf | MinInf, PlusInf -> MinInf
    | PlusInf, PlusInf | MinInf, MinInf -> PlusInf
    | Num 0, _ | _, Num 0 -> Num 0
    | PlusInf, Num _ | Num _, PlusInf -> PlusInf
    | MinInf, Num _ | Num _, MinInf -> MinInf

  let ( / ) n1 n2 =
    match (n1, n2) with
    | Num 0, _ -> Num 0
    | _, Num 0 -> raise (Error "Division by 0 not allowed!")
    | Num n1, Num n2 -> Num (n1 / n2)
    | PlusInf, MinInf | MinInf, PlusInf -> MinInf
    | PlusInf, PlusInf | MinInf, MinInf -> PlusInf
    | PlusInf, Num _ -> PlusInf
    | Num _, PlusInf -> Num 0
    | MinInf, Num _ -> MinInf
    | Num _, MinInf -> Num 0

  let ( <= ) n1 n2 =
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

  let ( > ) n1 n2 =
    match (n1, n2) with
    | Num n1, Num n2 -> n1 > n2
    | MinInf, PlusInf -> false
    | MinInf, MinInf -> true
    | MinInf, Num _ -> false
    | Num _, MinInf -> true
    | PlusInf, MinInf -> true
    | PlusInf, Num _ -> true
    | Num _, PlusInf -> false
    | PlusInf, PlusInf -> true

  let min l =
    let rec find_min l m =
      match l with
      | [] -> m
      | h :: t -> if h <= m then find_min t h else find_min t m
    in
    let m = List.hd l in
    find_min l m

  let max l =
    let rec find_max l m =
      match l with
      | [] -> m
      | h :: t -> if h <= m then find_max t m else find_max t h
    in
    let m = List.hd l in
    find_max l m
end
