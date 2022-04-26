(* ------------------------- Types ----------------------------- *)

type lettre =
  | DIGIT of (int)
  | UPPER of char
  | LOWER of char

(*  — nonemptystack -> lettre | lettre ; nonemptystack *)
type nonemptystack =
  | Lettre of lettre
  | Nonemptystack of lettre list

(* — stack -> vide | nonemptystack *)
type stack = 
  | Emptystack 
  | Stack of nonemptystack

(* — lettre-ou-vide -> vide | lettre *)
type lettre_ou_vide =
  | None
  | Some of lettre

(* — suitelettres-nonvide -> lettre | lettre , suitelettres-nonvide  *)
type suite_lettres_nonvide =
  | Lettre of lettre
  | SuiteLettresNonvide of lettre list

(* — transition -> ( lettre , lettre-ou-vide , lettre , lettre , stack ) *)
type transition =  lettre * lettre_ou_vide * lettre * lettre * stack

(* translist -> vide | transition translist *)
type translist = 
| Emptylist
| Translist of transition list

(* — transitions -> transitions: translist *)
type transitions = Transitions of translist

(* — initialstack -> initial stack symbol: lettre *) 
type initialstack = Initialstack of lettre
 
(* — initialstate -> initial state: lettre *) 
type initialstate = Initialstate of lettre

(*  — states -> states: suitelettres-nonvide *) 
type states = States of suite_lettres_nonvide

(*  — stacksymbols -> stack symbols: suitelettres-nonvide *)
type stacksymbols = Stacksymbols of suite_lettres_nonvide

(*  — inputsymbols -> input symbols: suitelettres-nonvide *)
type inputsymbols = Inputsymbols of suite_lettres_nonvide

(* — declarations -> inputsymbols stacksymbols states initialstate initialstack *)
type declarations = Declarations of inputsymbols * stacksymbols * states * initialstate * initialstack

(*  — automate -> declarations transitions *)
type automate = Automate of declarations * transitions


(* ----------------- As string ------------------------ *)

let lettre_as_string = function
  | UPPER c -> String.make 1 c  
  | LOWER c -> String.make 1 c
  | DIGIT c -> string_of_int c

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
  | SuiteLettresNonvide (sl_list) -> list_as_string (sl_list) (lettre_as_string) (",")
  )

let rec nonemptystack_as_string (n: nonemptystack) : string =
  (match n with
  | Lettre l -> lettre_as_string l
  | Nonemptystack (n_list) -> list_as_string (n_list) (lettre_as_string) (";")
  )

let stack_as_string (st: stack) : string =
  (match st with
  | Emptystack -> ""    
  | Stack (ne) -> nonemptystack_as_string ne  
  )
  
(* ----------------- Transition as string ------------------------ *)      
let transition_as_string (tr: transition) : string =
  let (l1, lv, l2, l3, s) = tr in 
  "(" ^ lettre_as_string l1 ^ lettre_ou_vide_as_string lv ^ lettre_as_string l2 ^ lettre_as_string l3 ^ stack_as_string s ^ ")"

 
let rec translist_as_string (tl: translist) : string =
  (match tl with
  | Emptylist -> ""
  | Translist (t_list: transition list) -> list_as_string (t_list) (transition_as_string) ("\n")
  )

let transitions_as_string (tr: transitions) : string =
  let (Transitions (tl)) = tr in 
  "transitions :" ^ "\n" ^ translist_as_string tl

let initial_stack_as_string (is: initialstack) : string =
  let (Initialstack (s)) = is in 
  "initial stack : " ^ lettre_as_string s

let initial_state_as_string (is: initialstate) : string =
  let (Initialstate (s)) = is in 
  "initial state : " ^ lettre_as_string s

let states_as_string (st: states) : string =
  let (States (s)) = st in 
  "states : " ^ suite_lettres_nonvide_as_string s

let stacksymbols_as_string (st: stacksymbols) : string =
  let (Stacksymbols (s)) = st in 
  "stack symbols : " ^ suite_lettres_nonvide_as_string s

let inputsymbols_as_string (st: inputsymbols) : string =
  let (Inputsymbols (s)) = st in 
  "input symbols : " ^ suite_lettres_nonvide_as_string s

let declarations_as_string (d: declarations) : string =
  let (Declarations (is, stsymb, st, inte, inck)) = d in 
  inputsymbols_as_string (is) ^ "\n" ^ stacksymbols_as_string (stsymb) ^ "\n" ^ states_as_string (st) ^ "\n" ^ initial_state_as_string (inte) ^ "\n" ^ initial_stack_as_string (inck)

let automate_as_string (a: automate) : string =
  let (Automate (d, tr)) = a in 
  declarations_as_string d ^ transitions_as_string tr
