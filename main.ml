open Prints
open Interpret

let usage () =
  print_string "Usage: ./grammaire [option] file [word] \n\n";
  print_string "Options: \n";
  print_string "--reprint     - compose abstract syntax tree based input file and reprint it\n"
  (*print_string "--interpret   - execute automaton based input file on word\n"*)

let exec_automate fun_grammaire fun_lexer fun_interpret file word =
(
  let ch = open_in file in

  let lexbuf = Lexing.from_channel ch in

  let ast = try
    Some (fun_grammaire fun_lexer lexbuf)
    with
      | msg -> (
        print_string (Printexc.to_string msg); print_string "\n";
        let pos = Lexing.lexeme_start_p lexbuf in
        Printf.fprintf stdout "At line %d, offset %d\n%!" pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
        None
      )
    in

  (match ast with
    | Some a -> 
    (
      try fun_interpret a (word_to_list word)
      with
      | InitialStateNotInList -> 
        print_string "automate non valide,\nl'état initial n'est pas dans la liste\n"
      | InitialStackNotInList -> 
        print_string "automate non valide,\nle symbole de pile initial n'est pas dans la liste\n"
      | NonDeterministicException -> 
        print_string "automate non valide,\ntransitions non déterministes\n"
      | _ -> print_string "erreur\n"
    )
    | None -> ()
  )
)

let main () =
  (match Sys.argv with
        | [|_;"--reprint";file|] -> 
          (
            let ch = open_in file in

            let lexbuf = Lexing.from_channel ch in

            let ast = try
              Some (Grammaire.automate Lexer.lexer lexbuf)
              with
                | msg -> (
                  print_string (Printexc.to_string msg); print_string "\n";
                  let pos = Lexing.lexeme_start_p lexbuf in
                  Printf.fprintf stdout "At line %d, offset %d\n%!" pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
                  None
                )
              in
          
            (match ast with
              | Some a -> Printf.printf "%s" (Prints.automate_as_string a)      
              | None -> ()
            )
          )

        | [|_;"-v2";file;word|] ->
          exec_automate Grammaire2.automate Lexer2.lexer Interpret2.execute_automate file word

        | [|_;"-v1";file;word|] ->
          exec_automate Grammaire.automate Lexer.lexer Interpret.execute_automate file word
          
        | _ -> usage()
  )

  let () = main ()