open Ast
open Prints

exception EmptyTransitionList of string
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

(* ------------------------- Main function ----------------------------- *)

let execute_automate (a : automate) (word : char list) : unit =  
  let Automate (decl, trs) = a in
  let Declarations (in_symb, st_symb, st, in_state, in_stack) = decl in
  
  let Initialstate (in_st) = in_state in  (* lettre *)
  let Initialstack (in_stck) = in_stack in  (* lettre *)

  let Transitions (trans_list) = trs in  
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
