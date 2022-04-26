let lexbuf = Lexing.from_channel stdin 

let ast = Parser.input Lexer.lexer lexbuf 

let _ = Printf.printf "%s" (Ast.as_string ast)
