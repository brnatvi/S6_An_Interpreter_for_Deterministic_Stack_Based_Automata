{
open Parser
}

let layout = [' ' '\n']
let digit = ['0'-'9']
let upper = ['A'-'Z']
let lower = ['a'-'z']

rule lexer = parse
  | layout            { lexer lexbuf }
  | ')'			          { RPAREN }
  | '('			          { LPAREN }
  | ";"		            { SEMI }
  | ","		            { COMMA }
  | "input symbols :" { INPUTSYMBOLS }
  | "stack symbols :" { STACKSYMBOLS }
  | "states :"        { STATES }
  | "initial state :" { INITIALSTATE }
  | "initial stack :" { INITIALSTACK }
  | "transitions :"   { TRANSITIONS }
  | digit             { DIGIT (Lexing.lexeme lexbuf) }
  | upper             { UPPER (Lexing.lexeme lexbuf) }
  | lower             { LOWER (Lexing.lexeme lexbuf) } 
  | eof			          { EOF }
  | _			            { failwith "unexpected character" }
