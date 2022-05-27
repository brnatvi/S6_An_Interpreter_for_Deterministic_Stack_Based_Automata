open Prints
open Prints2
open Interpret
open Interpret2

let usage () =
  print_string "Usage: ./grammaire [options] file [word] \n\n";
  print_string "Options: \n";
  print_string " --reprint   --v1  - reprint automaton described by transitions\n";
  print_string " --reprint   --v2  - reprint automaton described by program\n";
  print_string " --interpret --v1  - check if word is accepted by automaton described by\n";
  print_string "                     its transitions which is in file\n";
  print_string " --interpret --v2  - check if word is accepted by automaton described by\n";
  print_string "                     a program which is in file\n"

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
        print_string "Invalid automaton,\nthe initial state symbol is not in the list of state symbols\n"
      | InitialStackNotInList -> 
        print_string "Invalid automaton,\nthe initial stack symbol is not in list of stack symbols\n"
      | NonDeterministicException -> 
        print_string "Invalid automaton, non deterministic\n"
      | e -> (print_string (Printexc.to_string e); print_string "\n")
    )
    | None -> ()
  )
)

let reprint_automaton grammaire_fun lexer_fun print_fun file =
(
  let ch = open_in file in
  let lexbuf = Lexing.from_channel ch in
  let ast = try
    Some (grammaire_fun lexer_fun lexbuf)
    with
      | msg -> (
        print_string (Printexc.to_string msg);
        print_string "\n";
        let pos = Lexing.lexeme_start_p lexbuf in
        Printf.fprintf stdout "At line %d, offset %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
        None
      )
  in

  match ast with
    | Some a -> print_string (print_fun a)
    | None -> ()
  
)

let main () =
(
  match Sys.argv with
      | [|_;"--reprint";"--v1";file|] | [|_;"--v1";"--reprint";file|] -> 
        reprint_automaton Grammaire.automate Lexer.lexer Prints.automate_as_string_v1 file
      
      | [|_;"--reprint";"--v2";file|] | [|_;"--v2";"--reprint";file|] | [|_;"--reprint";file|] -> 
        reprint_automaton Grammaire2.automate Lexer2.lexer Prints2.automate_as_string_v2 file
      
      | [|_;"--interpret";"--v1";file;word|] | [|_;"--v1";"--interpret";file;word|] | [|_;"--v1";file;word|] ->
        exec_automate Grammaire.automate Lexer.lexer Interpret.execute_automate file word

      | [|_;"--interpret";"--v2";file;word|] | [|_;"--v2";"--interpret";file;word|] | [|_;"--v2";file;word|] | [|_;file;word|] ->
        exec_automate Grammaire2.automate Lexer2.lexer Interpret2.execute_automate file word
        
      | _ -> usage()
)

let () = main ()