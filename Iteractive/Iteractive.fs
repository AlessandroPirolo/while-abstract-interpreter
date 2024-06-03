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

let trap f =
    try f ()
    with UnexpectedError msg       -> printfn "\nunexpected error: %s" msg
        | ParseError (msg, last) -> printfn "\nparsing error: %s because of %s" msg last

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
        printf "%s\nnot recognized \"%s\" " message lastToken
        Skip

(*let rec print_res (stmt : Statement list) states (domain : AbstractDomain<_>) : string =
        match stmt, states with
        | h::t, h1::t1 ->
            "\n" + white_c + h.ToString + "\n" + yell_bold_c + domain.stateToString (h1) + white_c + print_res t t1 domain
        | [],[] -> ""
        | h::_, [] -> kprintf (fun s -> raise (UnexpectedError s)) "Error"
        | [], h::t -> "\n" + yell_bold_c + domain.stateToString (h) + print_res [] t domain*)

let start (domain : AbstractDomain<_>) = 
    printfn "Write the program or insert the its path: "
    let p = Console.ReadLine()
    let program = if p.Contains ".txt" then IO.File.ReadAllText p else p 
          
    let stmt = parse program
    let initial_state = domain.init_state stmt
    let result, invs = eval stmt initial_state domain
    if not result.IsEmpty then
        printfn "%s" (blue_c + "\nResult:" + white_c)
        printfn "initial state: %s" (yell_bold_c + domain.stateToString initial_state + white_c)
        printfn "final state: %s" (yell_bold_c + domain.stateToString result + white_c)
        printfn "%s" (yell_bold_c + invariants_to_string invs domain)
    else
        ()

let iteractive() = 
     while true do
        trap <| fun () ->
            printf "\n\x1b[1;32mWhileAbstract> %s" white_c
            stdout.Flush ()
            printfn "Choose the domain (1. intervals, 2. parametrized intervals, 3. sign): "
            let dom = Console.ReadLine()
            if dom.Trim() = "2" then
                printfn "Digit lower bound: "
                let lb = Console.ReadLine()  
                printfn "Digit upper bound: "
                let ub = Console.ReadLine()
                let lb = if lb.Trim() = "" then MinInf else Num (Parse lb) 
                let ub = if ub.Trim() = "" then PlusInf else Num (Parse ub) 
                let domain = new IntervalDomain(lb, ub)
                start domain
            else 
                ()
            
            if dom.Trim() = "1" then 
                let domain = new IntervalDomain(MinInf, PlusInf)
                start domain
            else
                () 

            if dom.Trim() = "3" then 
                let domain = new SignDomain()
                start domain
            else
                () 
            
            