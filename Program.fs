module Program

open Iteractive
    
[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        if argv.[0] = "help" then
            printfn "For running the interpreter do: WhileAbstractInterpreter.exe, then choose the domain and write your program"
    else
        iteractive()
               
    0
