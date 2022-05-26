open Ast2

exception EmptyStackException
exception EmptyList
exception InitStateNotFirst

exception EmptyStackException2
exception NonEmptyFinalStackException2
exception InstructionNotFound2
exception RejectException

type stack = lettre list

(* ------------------------- Processing functions ----------------------------- *)

(* Push *)
let push_to_stack (st: stack) (l: lettre) : stack =
(
  l::st
)


(* Pop *)
let pop_from_stack (st: stack) : stack =
(
  match st with
  | [] -> raise EmptyStackException2
  | _ :: tail -> tail
)


(* Change *)
let make_change l = failwith "TODO"
  


(* ------------------------- Auxilary functions ------------------------------- *)

let list_without_first word  =
  (
    match word with
    | [] -> raise EmptyList
    | _::tail -> tail
  )

let rec get_instr_from_switch_case (case_list : case list) (attribute:lettre) : (instruction option) = 
  match case_list with
  | [] -> None
  | Case(l,inst)::_ when l = attribute -> Some(inst)
  | _::tail -> get_instr_from_switch_case tail attribute


let rec execute_instruction (first_instr: instruction) (cur_instr: instruction) (cur_stack: stack) (cur_state: lettre) (cur_word: char list) : unit =
(
  match cur_instr with
  | Pop -> execute_instruction (first_instr) first_instr (pop_from_stack cur_stack) (cur_state) (cur_word) 
  | Push(l) -> execute_instruction (first_instr) first_instr (push_to_stack cur_stack l) (cur_state) (cur_word) 
  | Reject -> raise RejectException
  | Change(l) -> execute_instruction first_instr first_instr cur_stack l cur_word
  | PopAndChange (l) -> execute_instruction first_instr first_instr (pop_from_stack cur_stack) l cur_word
  | PushAndChange (l_st, l_e) -> execute_instruction first_instr first_instr (push_to_stack cur_stack l_st) l_e cur_word
  | SwitchCase (switch_case) -> 
  (
    match switch_case with
    | SwitchCaseState (case_list) ->( 
      let instr = get_instr_from_switch_case case_list cur_state in
      match instr with
      | None -> (
        match cur_stack, cur_word with
        | [],[] -> print_string "mot valide!"
        | _,_ -> raise InstructionNotFound2 
      )
      | Some i -> (
        execute_instruction first_instr i cur_stack cur_state cur_word
      )
    ) 
    | SwitchCaseNext (case_list) ->(
      let c = 
        try (List.hd cur_word) 
        with Failure x -> raise NonEmptyFinalStackException2
      in
      let instr = get_instr_from_switch_case case_list (Lower c) in
      match instr with
      | None -> (
        match cur_stack, cur_word with
        | [],[] -> print_string "mot valide!"
        | _,_ -> raise InstructionNotFound2 
      )
      | Some i -> (
        execute_instruction first_instr i cur_stack cur_state (list_without_first cur_word)
      )
    )
    | SwitchCaseTop (case_list) -> (
      let t = 
        try (List.hd cur_stack)
        with Failure x -> raise EmptyStackException2
      in
      let instr = get_instr_from_switch_case case_list t in
      match instr with
      | None -> (
        match cur_stack, cur_word with
        | [],[] -> print_string "mot valide!"
        | _,_ -> raise InstructionNotFound2 
      )
      | Some i -> (
        execute_instruction first_instr i cur_stack cur_state cur_word
      )
    )
  )
)

(* ------------------------- Main function ----------------------------- *)

let execute_automate (a : automate) (word : char list) : unit =
 (* failwith "todo"  *)
  (* initialisation *)
  let Automate (decl, instr) = a in

  (* dectaration part*)
  let Declarations (in_symb, st_symb, st, in_st, in_stck) = decl in

  let Initialstate (init_state) = in_st in  (* initial state initialisation (init_state) *)

  let Initialstack (in_stack) = in_stck in  
  let init_stack = [in_stack] in                 (* stack initialisation (stack) *)

  let States(states) = st in
  let Stacksymbols(stack_symbols) = st_symb in

  let Inputsymbols (input_symbols) = in_symb in

  (* instruction part*)
 (* let SwitchCase(switch_case) = instr in 
  let SwitchCaseState(switch_case_state) = switch_case in *)

  try 
    execute_instruction instr instr init_stack init_state word
  with
  | NonEmptyFinalStackException2 -> print_string "The word is not accepted,\ninput is empty, but stack isn't.\n"
  | EmptyStackException2 -> print_string "The word is not accepted,\nstack is empty, but input isn't.\n"
  | InstructionNotFound2 -> print_string "The word is not accepted,\nno transition has been applied.\n"
  | RejectException -> print_string "The word is not accepted,\nrejected by automaton.\n"
  | _ -> print_string "Error\n"    
  
