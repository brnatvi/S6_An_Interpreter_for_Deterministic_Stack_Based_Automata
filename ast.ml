(* ------------------------- Types ----------------------------- *)

type token =
  | RPAREN 
  | LPAREN
  | SEMI
  | COMMA

type lettre = 
  | DIGIT of (int)
  | UPPER of char
  | LOWER of char

type keywords =
  | INPUTSYMBOLS 
  | STACKSYMBOLS 
  | STATES
  | INITIALSTATE 
  | INITIALSTACK
  | TRANSITIONS

(*  — nonemptystack -> lettre | lettre ; nonemptystack *)
type nonemptystack =
  | Lettre of lettre
  | Nonemptystack of lettre * nonemptystack

(* — stack -> vide | nonemptystack *)
type stack = 
| [] 
| Stack of nonemptystack

(* — lettre-ou-vide -> vide | lettre *)
type lettre_ou_vide =
| None
| Some of lettre

(* — suitelettres-nonvide -> lettre | lettre , suitelettres-nonvide  *)
type suite_lettres_nonvide =
| Lettre of lettre
| SuiteLettresNonvide of lettre * suite_lettres_nonvide

(* — transition -> ( lettre , lettre-ou-vide , lettre , lettre , stack ) *)
type transition =  lettre * lettre_ou_vide * lettre * lettre * stack

(* translist -> vide | transition translist *)
type translist = 
| []
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


(* ----------------- Print ------------------------ *)

(* ----------------- auxilairy functions ------------------------ *)
let token_as_string = function
  | SEMI   -> ";"
  | COMMA  -> ","
  | LPAREN -> "("
  | RPAREN -> ")"

let lettre_as_string = function
  | UPPER c -> String.make 1 c  
  | LOWER c -> String.make 1 c
  | DIGIT c -> string_of_int c

let lettre_ou_vide_as_string c = 
  (match c with
  | None -> ""
  | Some c -> lettre_as_string c
  )

let keywords_as_string = function
  | INPUTSYMBOLS -> "input symbols :"
  | STACKSYMBOLS -> "stack symbols :"
  | STATES       -> "states :"
  | INITIALSTATE -> "initial state :"
  | INITIALSTACK -> "initial stack :"
  | TRANSITIONS  -> "transitions :" 

let rec suite_lettres_nonvide_as_string (s: suite_lettres_nonvide) : string =
  (match s with
  | Lettre l -> lettre_as_string l
  | SuiteLettresNonvide (l, slt) -> lettre_as_string l ^ suite_lettres_nonvide_as_string slt
  )

let rec nonemptystack_as_string (n: nonemptystack) : string =
    (match n with
    | Lettre k -> lettre_as_string k
    | Nonemptystack (k, ne) -> lettre_as_string k ^ nonemptystack_as_string ne
    )

let rec stack_as_string (st: stack) : string =
  (match st with
  | [] -> ""    
  | Stack s -> 
    (match s with 
    | Lettre c -> lettre_as_string c
    | Nonemptystack (l, ne) -> lettre_as_string l ^ nonemptystack_as_string ne
    )
  )
  
(* ----------------- Print transition ------------------------ *)      
let print_transition (tr: transition) : unit =
  let (l1, lv, l2, l3, s) = tr in 
  let str = "(" ^ lettre_as_string l1 ^ lettre_ou_vide_as_string lv ^ lettre_as_string l2 ^ lettre_as_string l3 ^ stack_as_string s ^ ")" in
  let _ = Printf.sprintf "%s\n" str in ()

 
let rec print_translist (tl: translist) : unit =
  match tl with
  | [] -> ()
  | Translist (t_list: transition list) ->
       (match t_list with
        | []  -> ()
        | h::tail -> print_transition h; print_string "\n"; print_list tail print_transition
       )

  and print_list l f =
    (match l with 
      | [] -> ()
      | x::xs -> f x; print_string "\n"; print_list xs f;
    )