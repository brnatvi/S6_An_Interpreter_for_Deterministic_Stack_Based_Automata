open Prints

let usage () =
  print_string "Usage: ./grammaire [option] pathfile \n\n";
  print_string "Options: \n";
  print_string "--reprint     - analyze input file and reprint program\n";
  print_string "--interpret   - execute input file\n"


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

        | [|_;"--interpret";file;word|] -> failwith "TODO"
        | _ -> usage()
  )

  let () = main ()