module Program

open Ast
open FSharp.Text.Lexing

(*let parse (ds : string) : Statement  = 
    let lexbuf = LexBuffer<char>.FromString ds
    Parser.prog Lexer.tokenize lexbuf
    *)
[<EntryPoint>]
let main argv =
    let c =  "x := 1; \n x := 1" 
    printfn "%s" c
    0
