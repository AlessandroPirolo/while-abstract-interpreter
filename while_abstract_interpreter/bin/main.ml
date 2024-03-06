open Interp

let parse (s : string) : Ast.statement =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.tokenize lexbuf in 
  ast

let () = let c = parse "x := 1; \n x := 1" in print_endline (Ast.to_string c)