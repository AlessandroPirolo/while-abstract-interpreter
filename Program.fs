module Program

open Ast
open FSharp.Text.Lexing
open Eval
open AbstractState

let rec invariants_to_string (invs : State Option list) : string =
    match invs with 
    | [] -> "\n"
    | h::t -> 
        let res = 
            match h with
            | Some inv -> "invariant 1: " + to_string inv 
            | None -> ""
        res + invariants_to_string t

let parse (ds : string) : Statement  = 
    let lexbuf = LexBuffer<char>.FromString ds
    Parser.prog Lexer.tokenize lexbuf
    
[<EntryPoint>]
let main argv =
    let program =  "x := 10; \n while x > 0 do x--  " 

    let stmt = parse program
    printfn "%s" stmt.ToString
    let initial_state = init_state stmt
    printfn "initial state %s" (to_string initial_state)
    let result, inv = eval stmt initial_state
    printfn "result %s" (to_string result)
    printfn "%s" (invariants_to_string inv)
    0
