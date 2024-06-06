module Iteractive

open Ast
open FSharp.Text.Lexing
open Eval
open IntervalDomain
open AbstractDomain
open type Number.Number
open type Interval.Interval
open System
open type System.Int32
open Printf
open SignDomain

exception UnexpectedError of string
exception ParseError of string * string

let white_c = "\x1b[0m"
let yell_bold_c = "\x1b[1;93m"
let blue_c = "\x1b[1;94m"
let red_c = "\x1b[1;31m"

let trap f =
    try f ()
    with UnexpectedError msg       -> printfn "\n%sunexpected error: %s %s"  red_c msg white_c
        | ParseError (_, last) -> printfn "\n%sparsing error: not recognized \"%s\" %s" red_c last white_c

let rec invariants_to_string (invs : Map<string, 'T> Option list) (domain : AbstractDomain<_>) : string =
    match invs with 
    | [] -> "\n"
    | h::t -> 
        let res = 
            match h with
            | Some inv -> white_c + "invariant: " + yell_bold_c + domain.stateToString inv  
            | None -> ""
        res + invariants_to_string t domain

let parse ds : Statement  = 
    let lexbuf = LexBuffer<_>.FromString ds
    try
        Parser.prog Lexer.tokenize lexbuf
    with e ->
        let message = e.Message
        let lastToken = new System.String(lexbuf.Lexeme)
        //printf "%s\nnot recognized \"%s\" " message lastToken
        raise (ParseError (message, lastToken))

(*let rec print_res (stmt : Statement list) states (domain : AbstractDomain<_>) : string =
        match stmt, states with
        | h::t, h1::t1 ->
            "\n" + white_c + h.ToString + "\n" + yell_bold_c + domain.stateToString (h1) + white_c + print_res t t1 domain
        | [],[] -> ""
        | h::_, [] -> kprintf (fun s -> raise (UnexpectedError s)) "Error"
        | [], h::t -> "\n" + yell_bold_c + domain.stateToString (h) + print_res [] t domain*)

let start (domain : AbstractDomain<_>) = 
    printfn "Choose the widening delay (small integer): "
    let wide_delay = Console.ReadLine()
    let wide_delay = try
                        if wide_delay.Trim() = "" then 1 else Parse wide_delay
                        with e ->
                                raise (UnexpectedError ("\"" + wide_delay + "\" is not a number " ))
    printfn "Write the program or insert the its path: "
    let p = Console.ReadLine()
    let program = if p.Contains ".txt" 
                    then 
                        let p = IO.File.ReadAllText p
                        printfn "\n %s" p
                        p
                    else p 
          
    let stmt = parse program
    let initial_state = domain.init_state stmt
    let result, invs = eval stmt initial_state domain wide_delay
    printfn "%s" (blue_c + "\nResult:" + white_c)
    printfn "initial state: %s" (yell_bold_c + domain.stateToString initial_state + white_c)
    printfn "final state: %s" (yell_bold_c + domain.stateToString result + white_c)
    printfn "%s" (yell_bold_c + invariants_to_string invs domain)

let iteractive() = 
     while true do
        trap <| fun () ->
            printf "\n\x1b[1;32mWhileAbstract> %s" white_c
            stdout.Flush ()
            printfn "Choose the domain (1. intervals, 2. parametrized intervals, 3. sign): "
            let dom = Console.ReadLine()
            match dom.Trim() with
            | "2" -> 
                printfn "Digit lower bound (press Enter to select minus infinte): "
                let lb = Console.ReadLine()  
                printfn "Digit upper bound (press Enter to select infinte): "
                let ub = Console.ReadLine()  
                let lb = try if lb.Trim() = "" then MinInf else Num (Parse lb) with e -> raise (UnexpectedError ("\"" + lb + "\" is not a number " ))
                let ub = try if ub.Trim() = "" then PlusInf else Num (Parse ub) with e -> raise (UnexpectedError ("\"" + ub + "\" is not a number " ))
                if lb >. ub 
                    then raise (UnexpectedError ("lower bound (" + lb.ToString + ") greater than upper bound (" + ub.ToString + ")"))
                    else 
                        let domain = new IntervalDomain(lb, ub)
                        start domain
            
            | "1" -> 
                let domain = new IntervalDomain(MinInf, PlusInf)
                start domain
            
            | "3" -> 
                let domain = new SignDomain()
                start domain
            
            | _ -> printfn "%s%s doesn't correspond to any implemented domain" yell_bold_c dom
       
            
            