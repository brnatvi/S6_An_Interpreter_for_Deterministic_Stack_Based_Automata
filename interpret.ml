open Ast
open Prints

exception EmptyTransitionList of string
exception TransitionNotFound
exception Empty of string
exception InitialStateCorrupted of string
exception InitialStackCorrupted of string
exception EmptyStackException
exception NonEmptyFinalStackException
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

(* GENERIC: delete last element from list, return list *)
let list_without_last l =
(
  let rev_list = List.rev l in
  match rev_list with
  | [] -> raise (Empty "List is empty")
  | [i] -> []
  | h::tail -> List.rev tail
)

(* GENERIC: pull last_but_one element from list *)
let rec get_last_but_one l =
(
  let rev_list = List.rev l in
    match rev_list with
    | [] -> raise Error
    | [i] -> i
    | h1::h2::tail -> h2
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
    | Emptystack -> print_string "mot valide!\n"
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

(* ------------------------- Main function ----------------------------- *)

let execute_automate (a : automate) (word : char list) : unit =
  let Automate (decl, trs) = a in
  let Declarations (in_symb, st_symb, st, in_state, in_stack) = decl in
  
  let Initialstate (in_st) = in_state in  (* lettre *)
  let Initialstack (in_stck) = in_stack in  (* lettre *)
  let Transitions (trans_l) = trs in  

  let initial_stack = Stack(in_stck, Emptystack) in

  let trans_list = 
    match trans_l with
    | Emptylist -> raise (EmptyTransitionList "empty transition list")
    | Translist(tl) -> tl
  in

  try
    go_to_next_state in_st word initial_stack trans_list
  with
    | NonEmptyFinalStackException -> print_string "mot non valide,\nl'entrée est épuisée sans que la pile soit vide.\n"
    | EmptyStackException -> print_string "mot non valide,\nla pile est vide sans que l'entrée soit épuisée.\n"
    | TransitionNotFound -> print_string "mot non valide,\naucune transition ne s'applique.\n"
    | _ -> print_string "erreur\n"

