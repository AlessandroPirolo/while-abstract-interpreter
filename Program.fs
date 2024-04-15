module Program

open Ast
open FSharp.Text.Lexing
open Eval
open AbstractState

let parse (ds : string) : Statement  = 
    let lexbuf = LexBuffer<char>.FromString ds
    Parser.prog Lexer.tokenize lexbuf
    
[<EntryPoint>]
let main argv =
    let program =  "y:= 3; x := 1; \n if x > 1 then x := 3 else x := 2" 
    
    let stmt = parse program
    printfn "%s" stmt.ToString
    let initial_state = init_state stmt
    printfn "initial state %s" (to_string initial_state)
    let result = eval stmt initial_state [initial_state]
    printfn "result %s" (to_string result)
    0
