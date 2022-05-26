open Ast2

exception EmptyStackException
exception EmptyList
exception InitStateNotFirst

exception EmptyStackException2
exception NonEmptyFinalStackException2
exception InstructionNotFound2

(* ------------------------- Processing functions ----------------------------- *)

(* Push *)
let push_to_stack (st: stack) (item: lettre) : stack =
  let Stack stak = st in
  (
    match stak with
    | [] -> Stack([])
    | h :: tail -> Stack(item :: (h :: tail))
  )


(* Pop *)
let pop_from_stack (st: stack) : stack =
  let Stack stak = st in
  (
    match stak with
    | [] -> raise EmptyStackException
    | h :: tail -> Stack(tail)
  )


(* Change *)
let make_change l = failwith "TODO"
  


(* ------------------------- Auxilary functions ------------------------------- *)

let list_without_last word  =
  (
    let rev_list = List.rev word in
    match rev_list with
    | [] -> raise ( EmptyList)
    | [i] -> []
    | h::tail -> List.rev tail
  )



let rec execute_instruction (cur_instr: instruction) (cur_stack: stack) (cur_state: lettre) (cur_word: char list) : (instruction * stack * lettre * lettre list ) =
(
  match cur_instr with
  | Pop -> execute_instruction (cur_instr) (pop_from_stack cur_stack) (cur_state) (list_without_last cur_word)       (* TODO cur_instr IS WRONG *)
  | Push(l) -> execute_instruction (cur_instr) (push_to_stack cur_stack l) (cur_state) (list_without_last cur_word)  (* TODO cur_instr IS WRONG *)
  | Reject -> failwith "Error"
  | Change(l) -> make_change l
  | PopAndChange (l) -> failwith "TODO"
  | PushAndChange (l_st, l_e) -> failwith "TODO"
  | SwitchCase (switch_case) -> 
    (
      match switch_case with
      | SwitchCaseState (case_liste) -> 
        (
          match case_liste with
          | [] -> failwith "TODO"
          | h::tail -> let Case(c, i) = h in
            (
              if (c != cur_state) then raise InitStateNotFirst
              else 
                let rez = execute_instruction i (cur_stack) (cur_state) (cur_word) in failwith "TODO"
            )
        )
            
            
      | SwitchCaseNext (case_liste) -> failwith "TODO"
       
      | SwitchCaseTop (case_liste) -> failwith "TODO"
    )
  )


let rec exicute_on_liste_instr (init_state: lettre) (init_stack : stack) (w: char list) (instr_list: instruction list) : (instruction * stack * lettre * lettre list ) =
    match instr_list with
    | [] -> failwith "TODO"
    | h :: tail -> failwith "TODO"
  
  

(* ------------------------- Main function ----------------------------- *)

let execute_automate (a : automate) (word : char list) : unit =
 (* failwith "todo"  *)
  (* initialisation *)
  let Automate (decl, instr) = a in

  (* dectaration part*)
  let Declarations (in_symb, st_symb, st, in_st, in_stck) = decl in

  let Initialstate (init_state) = in_st in  (* initial state initialisation (init_state) *)

  let Initialstack (in_stack) = in_stck in  
  let init_stack = Stack([in_stack]) in                 (* stack initialisation (stack) *)

  let States(states) = st in
  let Stacksymbols(stack_symbols) = st_symb in

  let Inputsymbols (input_symbols) = in_symb in

  (* instruction part*)
  let SwitchCase(switch_case) = instr in 
    let SwitchCaseState(instr_list) = switch_case in

    try 
        exicute_on_liste_instr (init_state) (init_stack) (word) (instr_list)
    with
    | NonEmptyFinalStackException2 -> print_string "The word is not accepted,\ninput is empty, but stack isn't.\n"
    | EmptyStackException2 -> print_string "The word is not accepted,\nstack is empty, but input isn't.\n"
    | InstructionNotFound2 -> print_string "The word is not accepted,\nno transition has been applied.\n"
    | _ -> print_string "Error\n"    
  
