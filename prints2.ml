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

(*let rec rest_stack_as_string (st: stack) : string =
  let Stack stak = st in
  (match stak with
  | [] -> ""
  | h :: tail -> rest_stack_as_string (Stack(tail)) ^ lettre_as_string h ^ ";"
  )

let stack_as_string (st: stack) : string =
  let Stack stak = st in
  (match stak with
    | [] -> ""
    | h :: tail -> rest_stack_as_string (Stack(tail)) ^ lettre_as_string h
)
*)


(* ----------------- Parts of definition as string ------------------------ *)


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

(* ----------------- Parts of definition as string ------------------------ *)

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

 

  (* -------------- Prints for etape 3 ---------------- *)

let rec instruction_as_string (i: instruction) : string =   
  ( 
    match i with
    | Pop -> "pop"
    | Push(lettre) -> "push " ^ lettre_as_string(lettre)
    | Reject -> "reject"
    | Change(lettre) -> "change" ^ lettre_as_string(lettre)
    | PopAndChange(lettre) -> "pop change " ^ lettre_as_string(lettre)  
    | PushAndChange(c, d) -> "push " ^ lettre_as_string(c) ^ " change " ^ lettre_as_string(d)    
    | SwitchCase(switch_case) -> 
      (
        match switch_case with
        | SwitchCaseState (case_list) -> 
          (
            match case_list with
            | [] -> "\n"
            | h :: tail -> failwith "TODO"                     
             (* let Case(int, instr1) = h in             
              "case state of\n    "^(lettre_as_string int)^": "^(instruction_as_string instr1)^"\n"^(list_as_string tail instruction_as_string"\n")
              *)
          )
        | SwitchCaseNext (case_list) -> 
          (
            match case_list with
            | [] -> "\n"
            | h :: tail -> failwith "TODO"    
               (* let Case(int, instr1) = h in 
                "case next of\n    begin:\n"^(lettre_as_string int )^": "^(instruction_as_string instr1)^"\n"^(list_as_string tail instruction_as_string "\n")^"\nend\n"
                *)
          )

        | SwitchCaseTop (case_list) -> 
          (
            match case_list with
            | [] -> "\n"
            | h :: tail -> failwith "TODO"  
             (* let Case(int, instr1) = h in 
              "case top of\n    begin:\n"^(lettre_as_string int )^": "^(instruction_as_string instr1)^"\n"^(list_as_string tail instruction_as_string "\n")^"\nend\n"
              *)
          )
      )      
  )

(* ----------------- Automat as string ------------------------ *) 

let automate_as_string_v2 (a: automate) : string =
  let Automate (d, inst) = a in 
    declarations_as_string d ^ instruction_as_string inst ^ "\n"