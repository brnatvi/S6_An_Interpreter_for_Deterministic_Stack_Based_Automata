%{
open Ast
%}

%token RPAREN LPAREN SEMI COMMA EOF
%token INPUTSYMBOLS STACKSYMBOLS STATES INITIALSTATE INITIALSTACK TRANSITIONS
%token <char> UPPER LOWER 
%token <int> DIGIT
%start <Ast.automate> automate

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
  d = DIGIT {lettre(d)}

lettre_maj:
  l = UPPER {lettre(l)}

(* lettre_min:
  LOWER {};
*)

suite_maj_nonvide:
  l = UPPER; COMMA; s = suite_maj_nonvide {SuiteLettresNonvide(l, s)} 
| l = UPPER {SuiteLettresNonvide(l)}

suite_min_nonvide:
  l = LOWER; COMMA; s = suite_min_nonvide {SuiteLettresNonvide(l, s)}
| l = LOWER {SuiteLettresNonvide(l)}

suite_num_nonvide:
  d = DIGIT; COMMA; s = suite_num_nonvide {SuiteLettresNonvide(d, s)}
| d = DIGIT {SuiteLettresNonvide (d)}

transitions:
  TRANSITIONS; tl = translist {Transitions(tl)}

translist:
  tr = transition; tl = translist {Translist(tr, tl)}
| {Emptylist}

transition:
  LPAREN; l1 = num; COMMA; l2 = lettre_min_ou_vide; COMMA; l3 = lettre_maj; COMMA; l4 = num; COMMA; s = stack; RPAREN {Transition(l1, l2, l3, l4, s)}

lettre_min_ou_vide:
  l = LOWER {Some(l)}
| {None}

stack:
  l = UPPER; SEMI; st = stack {Nonemptystack(l, st)}
| l = UPPER  {Nonemptystack(l)}
| {Emptystack}
