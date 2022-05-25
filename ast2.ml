
(* ------------------------- Abstract syntax tree ----------------------------- *)

type lettre =
  | Digit of int
  | Upper of char
  | Lower of char

(* — lettre-ou-vide -> vide | lettre *)
type lettre_ou_vide =
  | None
  | Some of lettre

(* — stack -> vide | nonemptystack *)
type stack = 
  | Emptystack 
  | Stack of lettre * stack

(* — suitelettres-nonvide -> lettre | lettre , suitelettres-nonvide  *)
type suite_lettres_nonvide =
  | Lettre of lettre
  | SuiteLettresNonvide of lettre * suite_lettres_nonvide

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

(*  — automate -> declarations instructions *)
type automate = Automate of declarations * instructions

type instructions = string (* TODO *)

(*
let inverse_stack (st : stack) : stack =
  let rec f st1 st2 =
    match st1 with
    | Emptystack -> st2
    | Stack(l,s) -> f s (Stack(l,st2))
  in
  f st Emptystack
*)