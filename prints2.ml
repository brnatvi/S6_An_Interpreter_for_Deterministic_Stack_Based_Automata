open Ast2

let lettre_as_string = function
  | Upper c -> String.make 1 c  
  | Lower c -> String.make 1 c
  | Digit c -> string_of_int c

let lettre_ou_vide_as_string c = 
  (match c with
  | None -> ""
  | Some c -> lettre_as_string c
  )

(* GENERIC: list as string with offset respected; delim = delimiter, f = function as_string for elements of list *)
let rec list_as_string list func delim offs =
  (match list with
  | [] -> ""
  | [x] -> func x offs
  | x::xs -> (func x offs) ^ delim ^ list_as_string (xs) (func) (delim) (offs)
  )

let rec suite_lettres_nonvide_as_string (s: suite_lettres_nonvide) : string =
  (match s with
  | Lettre l -> lettre_as_string l
  | SuiteLettresNonvide (l, li) -> lettre_as_string l ^ "," ^ suite_lettres_nonvide_as_string li
  )

(* ----------------- Definition as string ------------------------ *)

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

(* -------------- Part of instruction as string ---------------- *)

(* Offset as string, offset valie = 3 white spaces *)
let rec offset_as_string (depth:int) : string =
    match depth with
    | 0 -> ""
    | _ -> "   " ^ offset_as_string (depth - 1)
  
(* Instruction as string with offsets respected *)  
let rec instruction_as_string (i: instruction) (d: int) : string =   
  let rec case_as_string (c: case) (d: int)  : string =
    let Case(l, instr) = c in
    offset_as_string (d + 2)^ lettre_as_string l ^ ": " ^ instruction_as_string (instr) (d + 1) ^ "\n"
  in
  ( 
    match i with
    | Pop -> "pop"
    | Push(lettre) -> "push " ^ lettre_as_string(lettre)
    | Reject -> "reject"
    | Change(lettre) -> "change " ^ lettre_as_string(lettre)
    | PopAndChange(lettre) -> "pop change " ^ lettre_as_string(lettre)  
    | PushAndChange(c, d) -> "push " ^ lettre_as_string(c) ^ " change " ^ lettre_as_string(d)    
    | SwitchCase(switch_case) -> 
      (
        match switch_case with
        | SwitchCaseState (case_list) -> 
          (
            match case_list with
            | [] -> ""
            | h::tail -> offset_as_string (d+1)^"case state of\n" ^ case_as_string h (d) ^ (list_as_string tail case_as_string "" (d+1))
          )
        
        | SwitchCaseNext (case_list) -> 
          (
            match case_list with
            | [] -> ""
            | h::tail -> "begin\n" ^ offset_as_string (d+2)^ "case next of\n" ^ case_as_string h (d+1) ^ (list_as_string tail case_as_string "" (d+1))^(offset_as_string (d+2))^ "end" 
          )
        
        | SwitchCaseTop (case_list) -> 
          (
            match case_list with
            | [] -> ""
            | h::tail -> "begin\n"^ offset_as_string (d+2)^"case top of\n" ^ case_as_string h (d+1) ^ (list_as_string tail case_as_string "" (d+1)) ^(offset_as_string (d+2))^ "end" 
          )
      )      
  )

(* ----------------- Automat as string ------------------------ *) 

let automate_as_string_v2 (a: automate) : string =
  let Automate (d, inst) = a in 
    declarations_as_string d ^ "program:\n" ^ instruction_as_string inst 0