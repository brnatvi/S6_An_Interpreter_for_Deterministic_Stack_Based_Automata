%token RPAREN LPAREN SEMI COMMA EOF
%token INPUTSYMBOLS STACKSYMBOLS STATES INITIALSTATE INITIALSTACK TRANSITIONS
%token <char> UPPER LOWER 
%token <int> DIGIT
%start <void> automate
%%

automate:
  declarations; transitions; EOF {}


declarations:
  inputsymbols; stacksymbols; states; initialstate; initialstack {}

inputsymbols:
  INPUTSYMBOLS; suite_min_nonvide {}

stacksymbols:
  STACKSYMBOLS; suite_maj_nonvide {}

states:
  STATES; suite_num_nonvide {}

initialstate:
  INITIALSTATE; num {}

initialstack:
  INITIALSTACK; lettre_maj {}


num:
  DIGIT {}

lettre_maj:
  UPPER {}

(* lettre_min:
  LOWER {};
*)

suite_maj_nonvide:
  UPPER; COMMA; suite_maj_nonvide {} 
| UPPER {}

suite_min_nonvide:
  LOWER; COMMA; suite_min_nonvide {}
| LOWER {}

suite_num_nonvide:
  DIGIT; COMMA; suite_num_nonvide {}
| DIGIT {}


transitions:
  TRANSITIONS; translist {}

translist:
  transition translist {}
| {}

transition:
  LPAREN; DIGIT; COMMA; lettre_min_ou_vide; COMMA; 
  UPPER; COMMA; DIGIT; COMMA; pile; RPAREN {}

lettre_min_ou_vide:
  LOWER {}
| {}

pile:
  UPPER; SEMI; pile {}
| UPPER  {}
| {}









