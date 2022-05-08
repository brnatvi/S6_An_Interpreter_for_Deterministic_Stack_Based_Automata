open Ast
open Prints

exception EmptyTransitionList of string
exception TransitionNotFound of string
exception Empty of string
exception InitialStateCorrupted of string
exception InitialStackCorrupted of string
exception EmptyStackException
exception Error

(* ------------------------- Auxilairy functions ----------------------------- *)

(* transform string to list of char and return obtained list *)
let word_to_list (word : string) : char list =
  List.init (String.length word) (String.get word)

(* GENERIC: append element to list and return obtained list *)
let rec append l i =
  match l with
  | [] -> [i]
  | h::tail -> h::(append tail i)

(* GENERIC: pull last element from list *)
let rec pull_list l =
  match l with
  | [] -> raise (Empty "There is nothing to pull cause list is empty")
  | [i] -> i
  | h::tail -> pull_list tail

(* GENERIC: delete last element from list, return list *)
let list_without_last l =
  let rev_list = List.rev l in
  match rev_list with
  | [] -> raise (Empty "List is empty")
  | [i] -> []
  | h::tail -> List.rev tail

(* GENERIC: pull last_but_one element from list *)
let rec get_last_but_one l =  
  let rev_list = List.rev l in
    match rev_list with
    | [] -> raise (Error)
    | [i] -> i
    | h1::h2::tail -> h2
(* ------------------------- Service functions ----------------------------- *)

(*
(* pull last element from nonemptystack *)
let rec pull_from_nonemptystack (ne: nonemptystack) : char =
  match ne with 
  | Lettre l -> 
    (match l with
    | Digit i -> raise (Error)
    | Upper c -> c
    | Lower c -> c
    )
  | Nonemptystack (l, s) -> pull_from_nonemptystack s

(* pull last element from stack *)
let pull_from_stack (st: stack) : char =
  match st with
  | Emptystack -> raise (EmptyStackException)
  | Stack (ne) -> pull_from_nonemptystack ne

*)

let compare_char_lettre_ou_vide (st : lettre_ou_vide) (l: char) : bool =
  match st with  
    | None -> false
    | Some letter -> 
      (match letter with
      | Digit i -> raise (Error)
      | Upper c -> if c = l then true else false
      | Lower c -> if c = l then true else false
      )
 
let compare_char_lettre (st : lettre) (l: char) : bool =
  match st with
    | Digit i -> raise (Error)
    | Upper c -> if c = l then true else false
    | Lower c -> if c = l then true else false

let compare_two_lettres (s1 : lettre) (s2 : lettre) : bool =
  match s1, s1 with
    | Digit i1, Digit i2 -> if i1 = i2 then true else false 
    | Upper i1, Upper i2 -> if i1 = i2 then true else false 
    | Lower i1, Lower i2 -> if i1 = i2 then true else false  
    | _, _ -> false
    
let rec print_list (l: char list) : unit =
  match l with
  | [] -> print_char ']'
  | h::tail -> print_char '['; print_char h; print_char ' '; print_list tail

let get_char (st : lettre) : char =
  match st with
    | Digit i -> raise (Error)
    | Upper c -> c
    | Lower c -> c

let rec get_transition_from_state (curr_state : lettre) (c : lettre_ou_vide) (st : stack) (trans_list : transition list) : (transition) =
  let stack_start = 
    match st with
    | Emptystack -> raise EmptyStackException
    | Stack(l,_) -> l
  in
  
  match trans_list with
  | [] -> raise (TransitionNotFound "transition not found")
  | Transition(t_state, t_c, t_stack_start, t_next_state, t_stack)::li 
      (* je sais qu'on peut faire plus simplement mais je me souoviens pas de la syntaxe *)
      when(t_state = curr_state && t_c = c && t_stack_start = stack_start)
      -> Transition(t_state, t_c, t_stack_start, t_next_state, t_stack)
  | _::li -> get_transition_from_state curr_state c st li

(*
let inverse_stack (st : stack) : stack =
  let rec f st1 st2 =
    match st1 with
    | Emptystack -> st2
    | Stack(l,s) -> f s (Stack(l,st2))
  in
  f st Emptystack
*)

(* push all elements of s2 into s1 *)
let rec push_stack (st : stack) (st2 : stack) : stack =
  match st2 with
  | Emptystack -> st
  | Stack(l,s) -> push_stack (Stack(l,st)) s
  
let rec pop_n_push (st : stack) (st2 : stack) : stack =
  match st with
  | Emptystack -> raise EmptyStackException
  | Stack(l, s) -> push_stack s (inverse_stack st2)

let rec go_to_next_state (curr_state : lettre) (word : char list) (st : stack) (trans_list : transition list) : (unit) =
  match word with
  | [] -> (
    match st with
    | Emptystack -> print_string "mot valide!\n"
    | _ -> (
      let Transition(_,_,_,next_state, stack_end) = get_transition_from_state curr_state None st trans_list in
      go_to_next_state next_state word (pop_n_push st stack_end) trans_list
    )
  )
  | c::w -> (
    let Transition(_,_,_,next_state, stack_end) = get_transition_from_state curr_state (Some (Lower(c))) st trans_list in
    go_to_next_state next_state w (pop_n_push st stack_end) trans_list
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

  go_to_next_state in_st word initial_stack trans_list


  (*

  (match trans_list, word with  
  | Emptylist, _ -> raise (EmptyTransitionList ("Automate can't execute any word cause have not any transition"))
  | Translist (liste), word -> 
    let rec aux trans_l word curr_state curr_stack count =      (* curr_state : lettre,  curr_stack : list [] *)
      (match trans_l with
        | [] -> if (curr_stack = [] && word = []) then print_string "Word conform to automate" (*check if stack is empty and word = "" *)
                else print_string "Word doesn't conform to automate"
        | first_tr::rest_l -> 
          let Transition (depart, l_ou_v, nonterm, arrive, stack) = first_tr in
          let _ = print_int count in         

          (match word with
          | [] -> raise (Empty "This is empty word")
          | w::rest_word -> 
            if (compare_two_lettres (curr_state) (depart)) = false then raise(InitialStateCorrupted("Initial state != initial state of first transition"))
              else 
                (                        
                  let x = get_last_but_one curr_stack in 
                  let _ = Prints.lettre_as_string nonterm in 
                  let _ = print_string "nonterm" in
                  let _ = print_char x in
                  let _ = print_char '\n' in
                  
                  if (compare_char_lettre (nonterm) x) = false  then 
                    
                    raise(InitialStackCorrupted("Initial stack != initial stack of first transition"))
                  else
                    (
                      if (compare_char_lettre_ou_vide (l_ou_v) (w)) = false then aux rest_l word curr_state curr_stack (count +1)     (* continue with same letter and the rest of transitions *)
                      else
                        (                          
                          let new_stack = (try (append curr_stack (pull_from_stack stack)) with EmptyStackException -> list_without_last curr_stack) in  
                          let new_state = arrive in
                          let _ = Prints.lettre_as_string new_state in                           
                          let _ = print_list new_stack in
                          let _ = print_char '\n' in
                          aux rest_l rest_word new_state new_stack (count +1)
                        )
                    )
                )
          )
      )
    in aux liste word in_st [(get_char in_stck)] 0
  )

  *)