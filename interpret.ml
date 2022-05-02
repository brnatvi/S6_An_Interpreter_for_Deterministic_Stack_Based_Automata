open Ast

exception EmptyTransitionList of string
exception EmptyList of string

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
  | [] -> raise (EmptyList "There is nothing to pull cause list is empty")
  | [i] -> i
  | h::tail -> pull_list tail


(* ------------------------- Service functions ----------------------------- *)

(* check if stack empty*)
let is_stack_empty (l: transition list) : bool = failwith "TODO"
  


  

(* ------------------------- Main function ----------------------------- *)

let execute_automate (a : automate) (word : char list) : unit =
  let Automate (decl, trs) = a in
  let Transitions (trans_list) = trs in
  (match trans_list with
  | Emptylist -> raise (EmptyTransitionList ("Automate can't execute any word cause have not any transition"))
  | Translist (l) ->     
      (match l with
      | [] -> (*check if stack is empty and word = "" *) failwith "TODO"
      | x::xs -> failwith "TODO"
      
        (* let Transition (l1, l_ou_v, l2, l3, st) = x *)
          
      )
  )   