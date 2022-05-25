
(* ------------------------- Abstract syntax tree ----------------------------- *)

type lettre =
  | Digit of int
  | Upper of char
  | Lower of char

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

type case = Case of lettre * instruction

type switch_case =
  | SwitchCaseState of case list
  | SwitchCaseNext of case list
  | SwitchCaseTop of case list

type instruction = 
  | Pop
  | Push of lettre
  | Reject
  | Change of lettre
  | PopAndChange of lettre
  | PushAndChange of lettre * lettre (* symbole pile * nouvel état *)
  | SwitchCase of switch_case

(*  — automate -> declarations * première instruction *)
type automate = Automate of declarations * instruction


(*
let inverse_stack (st : stack) : stack =
  let rec f st1 st2 =
    match st1 with
    | Emptystack -> st2
    | Stack(l,s) -> f s (Stack(l,st2))
  in
  f st Emptystack
*)