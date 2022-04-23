%token RPAREN LPAREN SEMI COMMA EOF
%token INPUTSYMBOLS STACKSYMBOLS STATES INITIALSTATE INITIALSTACK TRANSITIONS
%token <char> UPPER LOWER 
%token <int> DIGIT
%start <void> automate
%%

automate:
  d = declarations; tr = transitions; EOF {Automate(d, tr)}


declarations:
  p1 = inputsymbols; p2 = stacksymbols; p3 = states; p4 = initialstate; p5 = initialstack {Declarations(p1, p2, p3, p4, p5)}

inputsymbols:
  INPUTSYMBOLS; isb = suite_min_nonvide {Inputsymbols(isb)}

stacksymbols:
  STACKSYMBOLS; stsmb = suite_maj_nonvide {Stacksymbols(stsmb)}

states:
  STATES; s = suite_num_nonvide {States(s)}

initialstate:
  INITIALSTATE; ist = num {Initialstate(ist)}

initialstack:
  INITIALSTACK; isk = lettre_maj {Initialstack(isk)}


num:
  DIGIT {}

lettre_maj:
  UPPER {}

(* lettre_min:
  LOWER {};
*)

suite_maj_nonvide:
  u = UPPER COMMA s = suite_maj_nonvide {} 
| UPPER {}

suite_min_nonvide:
  LOWER; COMMA; suite_min_nonvide {}
| LOWER {}

suite_num_nonvide:
  DIGIT; COMMA; suite_num_nonvide {}
| DIGIT {}


transitions:
  TRANSITIONS; tl = translist {Transitions(tl)}

translist:
  transition translist {}
| {}

transition:
  LPAREN; l1 = lettre_min; COMMA; l2 = lettre_min_ou_vide; COMMA; l3 = lettre_min; COMMA; l3 = lettre_min; COMMA; s = stack; RPAREN {Transition(l1, l2, l3, l4, s)}

lettre_min_ou_vide:
  LOWER {}
| {}

stack:
  UPPER; SEMI; stack {}
| UPPER  {}
| {}









