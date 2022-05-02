let ch = open_in (Sys.argv.(1)) in

let lexbuf = Lexing.from_channel ch in

let ast =
try
  Some (Grammaire.automate Lexer.lexer lexbuf)
with
  | msg -> (
    print_string (Printexc.to_string msg); print_string "\n";
    let pos = Lexing.lexeme_start_p lexbuf in
    Printf.fprintf stdout "At line %d, offset %d\n%!" pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
    None
  )
in

match ast with
  | Some a -> Printf.printf "%s" (Ast.automate_as_string a)
  | None -> ()