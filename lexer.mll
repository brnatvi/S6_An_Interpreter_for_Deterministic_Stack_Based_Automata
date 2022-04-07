{
open Parser
}

let white = [' ']
let newline = ['\r'] | ['\n'] | ["\r\n"]    (* which one for Linux ? *)
let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let lettre = digit | upper | lower

let keywords =
[
  "input symbols :", INPUT;
  "stack symbols :", STACKSYMB;
  "states :", STATES;
  "initial state :", INSTACK;
  "initial stack :", INSTATE;
  "transitions :", TRANS; 
]

let next_line lexbuf =
  failwith "TODO"


rule main = parse
  | white     { main lexbuf }
  | newline   { next_line lexbuf; main lexbuf }
  | ')'			  { RPAREN }
  | '('			  { LPAREN }
  | ";"		    { SEMI }
  | ","		    { COMMA }
  | lettre    { LETTER (Lexing.lexeme lexbuf) }
  | keywords  { KW (Lexing.lexeme lexbuf) } 
  | eof			  { EOF }
  | _			    { failwith "unexpected character" }
