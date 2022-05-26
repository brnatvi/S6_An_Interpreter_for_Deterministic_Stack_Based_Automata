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

(* GENERIC: append element to list and return obtained list *)
let rec append l i =
(
  match l with
  | [] -> [i]
  | h::tail -> h::(append tail i)
)

(* GENERIC: pull last element from list *)
let rec pull_list l =
(
  match l with
  | [] -> raise (Empty "There is nothing to pull cause list is empty")
  | [i] -> i
  | h::tail -> pull_list tail
)

(* ------------------------- Service functions ----------------------------- *)

let compare_char_lettre_ou_vide (st : lettre_ou_vide) (l: char) : bool = 
(
  match st with  
    | None -> false
    | Some letter -> 
      (
        match letter with
        | Digit i -> raise (Error)
        | Upper c -> if c = l then true else false
        | Lower c -> if c = l then true else false
      )
)

let compare_char_lettre (st : lettre) (l: char) : bool = 
(
  match st with
    | Digit i -> raise (Error)
    | Upper c -> if c = l then true else false
    | Lower c -> if c = l then true else false
)

let compare_two_lettres (s1 : lettre) (s2 : lettre) : bool =
(
  match s1, s1 with
    | Digit i1, Digit i2 -> if i1 = i2 then true else false 
    | Upper i1, Upper i2 -> if i1 = i2 then true else false 
    | Lower i1, Lower i2 -> if i1 = i2 then true else false  
    | _, _ -> false
)
    
let rec print_list (l: char list) : unit =
(
  match l with
  | [] -> print_char ']'
  | h::tail -> print_char '['; print_char h; print_char ' '; print_list tail
)

let get_char (st : lettre) : char =
(
  match st with
    | Digit i -> raise (Error)
    | Upper c -> c
    | Lower c -> c
)

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
      (* je sais qu'on peut faire plus simplement mais je me souoviens pas de la syntaxe *)
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

let rec go_to_next_state (curr_state : lettre) (word : char list) (st : stack) (trans_list : transition list) : (unit) =
(
  match word with
  | [] -> 
  (
    match st with
    | Emptystack -> print_string "This word is accepted by automate !\n"
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

let rec is_in_lettre_list (list : suite_lettres_nonvide) (e : lettre) : (bool) =
(
  match list with
  | SuiteLettresNonvide (l, rest) ->
    if l = e then true 
    else is_in_lettre_list rest e
  | Lettre(l) -> 
    l = e
)

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

  (* initialisation *)
  let Automate (decl, trs) = a in
  let Declarations (in_symb, st_symb, st, in_state, in_stack) = decl in
  
  let Initialstate (in_st) = in_state in  (* lettre *)
  let Initialstack (in_stck) = in_stack in  (* lettre *)
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

  (* execution de l'automate *)
  try
    go_to_next_state in_st word initial_stack trans_list
  with
    | NonEmptyFinalStackException -> print_string "The word is not accepted,\ninput is empty, but stack isn't.\n"
    | EmptyStackException -> print_string "The word is not accepted,\nstack is empty, but input isn't.\n"
    | TransitionNotFound -> print_string "The word is not accepted,\nno transition has been applied.\n"
    | _ -> print_string "Error\n"

