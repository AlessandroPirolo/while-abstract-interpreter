exception Error of string

type number = 
  | MinInf
  | PlusInf
  | Num of int

(* Operations on numbers *)

let (+) n1 n2 = match (n1, n2) with
  | Num _, MinInf -> 
  | MinInf, Num _ -> MinInf
  | Num _, PlusInf -> 
  | PlusInf, Num _ -> PlusInf
  | Num n1, Num n2 -> Num (n1 + n2)
  | PlusInf, PlusInf -> PlusInf 
  | MinInf, MinInf -> MinInf
  | MinInf, PlusINf ->
  | PlusInf, MinInf -> PlusInf

let (-) n1 n2 = match (n1, n2) with
  | Num _, MinInf -> 
  | MinInf, Num _ -> MinInf
  | Num _, PlusInf -> 
  | PlusInf, Num _ -> PlusInf
  | Num n1, Num n2 -> Num (n1 - n2)
  | PlusInf, PlusInf -> PlusInf 
  | MinInf, MinInf -> MinInf
  | MinInf, PlusINf ->
  | PlusInf, MinInf -> PlusInf

let (!-) n = match n with
  | Num n -> Num ((-1) * n)
  | PluInf -> MinInf
  | MinInf -> PlusInf

let ( * ) n1 n2 = match (n1, n2) with
  | Num n1, Num n2 -> Num (n1 * n2)
  | PlusInf, MinInf
  | MinInf, PlusInf -> MinInf
  | PlusInf, PlusInf
  | MinInf, MinInf -> PlusInf
  | Num 0, _ 
  | _, Num 0 -> Num 0
  | PlusInf, Num _ 
  | Num _, PlusInf -> PlusInf
  | MinInf, Num _
  | Num _, MinInf -> MinInf

let (/) n1 n2 = match (n1, n2) with
  | Num 0, _ -> Num 0
  | _, Num 0 -> raise (Error "Division by 0 not allowed!")
  | Num n1, Num n2 -> Num (n1 / n2)
  | PlusInf, MinInf
  | MinInf, PlusInf -> MinInf
  | PlusInf, PlusInf
  | MinInf, MinInf -> PlusInf
  | PlusInf, Num _ -> PlusInf
  | Num _, PlusInf -> Num 0
  | MinInf, Num _ -> MinInf
  | Num _, MinInf -> Num 0

let (<=) n1 n2 = match (n1, n2) with
  | Num n1, Num n2 -> n1 <= n2
  | MinInf, PlusInf -> true
  | MinInf, MinInf -> true
  | MinInf, Num n -> true
  | Num n, MinInf -> false
  | PlusInf, MinInf -> false
  | PlusInf, Num n -> false
  | Num n, PlusInf -> true
  | PlusInf, PlusInf -> true

let min l =
  let m = List.hd l
  let rec find_min l m =
    match l with
    | [] -> m
    | h :: t -> if h <= m then find_min t h else find_min t m  
  find_min l m

let max l = 
  let m = List.hd l
  let rec find_max l m =
    match l with
    | [] -> m
    | h :: t -> if h <= m then find_max t m else find_max t h  
  find_max l m


type interval =
  | Interval of number * number
  | Empty
  | Z

(* Operations on interval *)
let lub x y = 
  match (x, y) with
  | Interval (a,b), Interval (c,d) -> Interval (min a c, max b d)
  | _, _ -> Empty

let glb x y = match (x, y) with
  | Interval (a,b), Interval (c,d) -> 
      let ma = max [a; c] 
      let mi = min [b; d]
      if ma <= mi then
        Interval (ma, mi)
      else
        Empty
  | _, _ -> Empty

let (!-) i = match i with
  | Interval (a,b) = Interval (-b, -a)
  | _ -> Empty

let (+) x y = 
  match (x, y) with
  | Interval (a,b), Interval (c,d) -> Interval (a + c, b + d)
  | _, _ -> Empty

let (-) x y = 
  match (x, y) with
  | Interval (a,b), Interval (c,d) -> Interval (a - d, b - c)
  | _, _ -> Empty

let ( * ) x y = 
  match (x, y) with
  | Interval (a,b), Interval (c,d) -> 
    let ac = a * c 
    let ad = a * d
    let bc = b * c 
    let bd = b * d 

    let mi = min [ac; ad; bc; bd]
    let ma = max [ac; ad; bc; bd]

    Interval (mi, ma)
  | _, _ -> Empty

let (/) x y = 
  match (x, y) with
  | Interval (a,b), Interval (c,d) -> 
    let ac = a / c 
    let ad = a / d
    let bc = b / c 
    let bd = b / d 

    if Num 1 <= c then 
      let mi = min [ac; ad]
      let ma = max [bc; bd]
      Interval (mi, ma)
    else if d <= Num (-1) then
      let ma = max [ac; ad]
      let mi = min [bc; bd]
      Interval (mi, ma)
    else lub (x / (glb y Interval (Num 1, PlusInf))) (x / (glb y Interval (MinInf, Num (-1))))

    Interval (mi, ma)
  | _, _ -> Empty
 

class interval_domain (m : int) (n : int) =
  object (self)
    val mutable state : (string, int) Hashtbl.t = Hashtbl.create 0
    
  end