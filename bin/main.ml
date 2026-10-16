open Hake

let () =
  let _ = Parser.prog Lexer.read (Lexing.from_string "var x = 5") in ()
