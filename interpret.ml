open Ast
open Prints

exception EmptyTransitionList of string
exception TransitionNotFound
exception Empty of string
exception InitialStateNotInList
exception InitialStackNotInList
exception EmptyStackException
exception NonEmptyFinalStackException
exception NonDeterministicException
exception Error

(* ------------------------- Auxilairy functions ----------------------------- *)

(* transform string to list of char and return obtained list *)
let word_to_list (word : string) : char list =
  List.init (String.length word) (String.get word)

(* return transition according state *)
let rec get_transition_from_state (curr_state : lettre) (c : lettre_ou_vide) (st : stack) (trans_list : transition list) : (transition option) =
(
  let stack_start = 
    match st with
    | Emptystack -> raise EmptyStackException
    | Stack(l,_) -> l
  in
  
  match trans_list with
  | [] -> None
  | Transition(t_state, t_c, t_stack_start, t_next_state, t_stack)::li     
      when(t_state = curr_state && t_c = c && t_stack_start = stack_start)
      -> Some (Transition(t_state, t_c, t_stack_start, t_next_state, t_stack))
  | _::li -> get_transition_from_state curr_state c st li
)

(* push all elements of s2 into s1 *)
let rec push_stack (st : stack) (st2 : stack) : stack =
(
  match st2 with
  | Emptystack -> st
  | Stack(l,s) -> push_stack (Stack(l,st)) s
)

let rec pop_n_push (st : stack) (st2 : stack) : stack =
(
  match st with
  | Emptystack -> raise EmptyStackException
  | Stack(l, s) -> push_stack s (inverse_stack st2)
)

(* traversal by list of transitions*)
let rec go_to_next_state (curr_state : lettre) (word : char list) (st : stack) (trans_list : transition list) : (unit) =
(
  match word with
  | [] -> 
  (
    match st with
    | Emptystack -> print_string "This word is accepted by automaton !\n"
    | _ -> 
    (
      let trans_opt = get_transition_from_state curr_state None st trans_list in
      match trans_opt with
      | None -> raise NonEmptyFinalStackException
      | Some Transition(_,_,_,next_state, stack_end) -> (* transition epsilon *)
        go_to_next_state next_state word (pop_n_push st stack_end) trans_list
    )
  )
  | c::rest_word -> 
  (
    match st with
    | Emptystack -> raise EmptyStackException
    | _ -> 
    (
      let trans_opt = get_transition_from_state curr_state (Some (Lower(c))) st trans_list in
      match trans_opt with
      | None -> 
      (
        let trans_eps_opt = get_transition_from_state curr_state None st trans_list in
        match trans_eps_opt with
        | None -> raise TransitionNotFound
        | Some Transition(_,_,_,next_state, stack_end) -> (* transition epsilon *)
          go_to_next_state next_state word (pop_n_push st stack_end) trans_list
      )
      | Some Transition(_,_,_,next_state, stack_end) -> (* transition consummant une lettre *)
        go_to_next_state next_state rest_word (pop_n_push st stack_end) trans_list
    )
  )
)

(* ------------------------- Error check functions ----------------------------- *)

(* check if init_state is in list of states*)
let rec is_in_lettre_list (list : suite_lettres_nonvide) (e : lettre) : (bool) =
(
  match list with
  | SuiteLettresNonvide (l, rest) ->
    if l = e then true 
    else is_in_lettre_list rest e
  | Lettre(l) -> 
    l = e
)

(* check if autonaton is not deterministic *)
let rec is_deterministic (trans_list : transition list) : bool = 
(
  let rec f list t =
  (
    match list with
    | [] -> true
    | t2::rest -> 
    (
      let Transition (s, c, st, _, _) = t in
      let Transition (s2, c2, st2, _, _) = t2 in
      if (s=s2 && st=st2) then
        match c,c2 with
        | None, _ -> (
          print_string ((transition_as_string t) ^ "\n" ^ (transition_as_string t2) ^ "\n");
          false)
        | _, None -> (
          print_string ((transition_as_string t) ^ "\n" ^ (transition_as_string t2) ^ "\n");
          false)
        | Some(x), Some(y) when x=y -> (
          print_string ((transition_as_string t) ^ "\n" ^ (transition_as_string t2) ^ "\n");
          false)
        | _ -> f rest t
      else 
        f rest t
    )
  )
  in
  match trans_list with
  | [] -> true
  | t::list -> 
  (
    if not (f list t) then false
    else is_deterministic list
  )
)

(* ------------------------- Main function ----------------------------- *)

let execute_automate (a : automate) (word : char list) : unit =

  (* declarations *)
  let Automate (decl, trs) = a in
  let Declarations (in_symb, st_symb, st, in_state, in_stack) = decl in
  
  let Initialstate (in_st) = in_state in      (* lettre *)
  let Initialstack (in_stck) = in_stack in    (* lettre *)
  let Transitions (trans_l) = trs in  

  let initial_stack = Stack(in_stck, Emptystack) in

  let trans_list = 
    match trans_l with
    | Emptylist -> raise (EmptyTransitionList "Empty transition list")
    | Translist(tl) -> tl
  in

  let States(states) = st in
  let Stacksymbols(stack_symbols) = st_symb in

  (* verifications de la bonne formation de l'automate *)
  if not (is_in_lettre_list states in_st)
  then raise InitialStateNotInList
  else

  if not (is_in_lettre_list stack_symbols in_stck)
  then raise InitialStackNotInList
  else

  if not (is_deterministic trans_list)
  then raise NonDeterministicException
  else

  (* execution of automaton *)
  try
    go_to_next_state in_st word initial_stack trans_list
  with
    | NonEmptyFinalStackException -> print_string "Word not accepted,\ninput is empty, but stack isn't.\n"
    | EmptyStackException -> print_string "Word not accepted,\nstack is empty, but input isn't.\n"
    | TransitionNotFound -> print_string "Word not accepted,\nno transition can be applied.\n"
    | _ -> print_string "Error\n"