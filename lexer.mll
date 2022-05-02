{
  open Grammaire
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']

rule lexer = parse
  | white             { lexer lexbuf }
  | newline           { Lexing.new_line lexbuf; lexer lexbuf }
  | ')'			          { RPAREN }
  | '('			          { LPAREN }
  | ";"		            { SEMI }
  | ","		            { COMMA }
  | "input symbols:"  { INPUTSYMBOLS }
  | "stack symbols:"  { STACKSYMBOLS }
  | "states:"         { STATES }
  | "initial state:"  { INITIALSTATE }
  | "initial stack:"  { INITIALSTACK }
  | "transitions:"    { TRANSITIONS }
  | digit as d        { DIGIT (Char.code d - Char.code '0') }
  | upper as c        { UPPER (c) }
  | lower as c        { LOWER (c) } 
  | eof			          { EOF }
  | _			            { failwith "unexpected character" }
