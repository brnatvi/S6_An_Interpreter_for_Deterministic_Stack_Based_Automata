let lexbuf = Lexing.from_channel stdin 

let ast = Parser.input Lexer.lexer lexbuf 

let _ = Printf.printf "Parse:\n%s\n" (Ast.as_string ast)
