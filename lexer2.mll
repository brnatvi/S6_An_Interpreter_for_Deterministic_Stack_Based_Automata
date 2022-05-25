{
  open Grammaire2
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']+
let upper = ['A'-'Z']
let lower = ['a'-'z']
let case_digit = digit":"
let case_upper = upper":"
let case_lower = lower":"

rule lexer = parse
  | white             { lexer lexbuf }
  | newline           { Lexing.new_line lexbuf; lexer lexbuf }
  | ","		            { COMMA }
  | "input symbols:"  { INPUTSYMBOLS }
  | "stack symbols:"  { STACKSYMBOLS }
  | "states:"         { STATES }
  | "initial state:"  { INITIALSTATE }
  | "initial stack:"  { INITIALSTACK }
  | "program:"        { PROGRAM }
  | digit as d        { DIGIT (int_of_string d) }
  | upper as c        { UPPER (c) }
  | lower as c        { LOWER (c) }
  | digit as d":"     { CASE_DIGIT (int_of_string d) }
  | upper as c":"     { CASE_UPPER (c) }
  | lower as c":"     { CASE_LOWER (c) }
  | "state"           { STATE }
  | "top"             { TOP }
  | "next"            { NEXT }
  | "case"            { CASE }
  | "of"              { OF }
  | "pop"             { POP }
  | "push"            { PUSH }
  | "change"          { CHANGE }
  | "reject"          { REJECT }
  | "begin"           { BEGIN }
  | "end"             { END }
  | eof			          { EOF }
  | _			            { failwith "unexpected character" }
