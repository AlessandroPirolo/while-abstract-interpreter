module Program

open Ast
open FSharp.Text.Lexing
open Eval
open AbstractState
open IntervalDomain
open type Number.Number
open type System.Int32

exception UnexpectedError of string

let rec invariants_to_string (invs : State Option list) : string =
    match invs with 
    | [] -> "\n"
    | h::t -> 
        let res = 
            match h with
            | Some inv -> "invariant: " + to_string inv 
            | None -> ""
        res + invariants_to_string t

let parse ds : Statement  = 
    let lexbuf = LexBuffer<_>.FromString ds
    Parser.prog Lexer.tokenize lexbuf
    
[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        if argv.[0] = "help" then
            printfn "For running the interpreter do: WhileAbstractInterpreter.exe [name of text file containing the program] [lower bound] [upper bound]"
        else
            let lb = if argv.Length > 1 then Num (Parse argv.[1]) else MinInf
            let ub = if argv.Length > 2 then Num (Parse argv.[2]) else PlusInf 
            let domain = new IntervalDomain(lb, ub)
            let program = if argv.[0].Contains ".txt" then System.IO.File.ReadAllText argv.[0] else argv.[0]
         
            let stmt = parse program
            let initial_state = init_state stmt
            printfn "initial state %s" (to_string initial_state)
            let result, inv = eval stmt initial_state domain
            printfn "result %s" (to_string result)
            printfn "%s" (invariants_to_string inv)   
    else
        printfn "Type \"help\" after calling the program"
    0
