open Ast


let lettre_as_string = function
  | Upper c -> String.make 1 c  
  | Lower c -> String.make 1 c
  | Digit c -> string_of_int c

let lettre_ou_vide_as_string c = 
  (match c with
  | None -> ""
  | Some c -> lettre_as_string c
  )

(* Generic function: return generic list as string with delim as delimiter, f = function as_string for elements of list *)
let rec list_as_string list func delim =
  (match list with
  | [] -> ""
  | [x] -> func x
  | x::xs -> func x ^ delim ^ list_as_string (xs) (func) (delim)
  )

let rec suite_lettres_nonvide_as_string (s: suite_lettres_nonvide) : string =
  (match s with
  | Lettre l -> lettre_as_string l
  | SuiteLettresNonvide (l, li) -> lettre_as_string l ^ "," ^ suite_lettres_nonvide_as_string li
  )

let rec rest_stack_as_string (st: stack) : string =
  (match st with
  | Emptystack -> ""
  | Stack (l, s) -> rest_stack_as_string s ^ lettre_as_string l ^ ";"
  )

let stack_as_string (st: stack) : string =
  (match st with
  | Emptystack -> ""
  | Stack (l, s) -> rest_stack_as_string s ^ lettre_as_string l
  )
  
(* ----------------- Transition and its list as string ------------------------ *)

let transition_as_string (tr: transition) : string =
  let Transition (l1, lv, l2, l3, s) = tr in 
  "(" ^ lettre_as_string l1 ^ "," ^
  lettre_ou_vide_as_string lv ^ "," ^
  lettre_as_string l2 ^ "," ^
  lettre_as_string l3 ^ "," ^
  stack_as_string s ^ ")"

 
let rec translist_as_string (tl: translist) : string =
  (match tl with
  | Emptylist -> ""
  | Translist (t_list: transition list) -> list_as_string (t_list) (transition_as_string) ("\n")
  )

(* ----------------- Parts of definition as string ------------------------ *)

let transitions_as_string (tr: transitions) : string =
  let (Transitions (tl)) = tr in 
  "transitions:\n\n" ^ translist_as_string tl

let initial_stack_as_string (is: initialstack) : string =
  let (Initialstack (s)) = is in 
  "initial stack: " ^ lettre_as_string s

let initial_state_as_string (is: initialstate) : string =
  let (Initialstate (s)) = is in 
  "initial state: " ^ lettre_as_string s

let states_as_string (st: states) : string =
  let (States (s)) = st in 
  "states: " ^ suite_lettres_nonvide_as_string s

let stacksymbols_as_string (st: stacksymbols) : string =
  let (Stacksymbols (s)) = st in 
  "stack symbols: " ^ suite_lettres_nonvide_as_string s

let inputsymbols_as_string (st: inputsymbols) : string =
  let (Inputsymbols (s)) = st in 
  "input symbols: " ^ suite_lettres_nonvide_as_string s

let declarations_as_string (d: declarations) : string =
  let (Declarations (is, stsymb, st, inte, inck)) = d in 
  inputsymbols_as_string (is) ^ "\n" ^
  stacksymbols_as_string (stsymb) ^ "\n" ^
  states_as_string (st) ^ "\n" ^
  initial_state_as_string (inte) ^ "\n" ^
  initial_stack_as_string (inck) ^ "\n\n"

(* ----------------- Automat as string ------------------------ *) 
let automate_as_string (a: automate) : string =
  let (Automate (d, tr)) = a in 
  declarations_as_string d ^ transitions_as_string tr ^ "\n"