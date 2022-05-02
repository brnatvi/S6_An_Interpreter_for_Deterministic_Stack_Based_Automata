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
  INITIALSTATE; ist = digit {Initialstate(ist)}

initialstack:
  INITIALSTACK; isk = upper {Initialstack(isk)}

digit:
  d = DIGIT {Digit(d)}

upper:
  l = UPPER {Upper(l)}

lower:
  l = LOWER {Lower(l)}

suite_maj_nonvide:
  l = upper; COMMA; s = suite_maj_nonvide {SuiteLettresNonvide(l, s)} 
| l = upper {Lettre(l)}

suite_min_nonvide:
  l = lower; COMMA; s = suite_min_nonvide {SuiteLettresNonvide(l, s)}
| l = lower {Lettre(l)}

suite_num_nonvide:
  d = digit; COMMA; s = suite_num_nonvide {SuiteLettresNonvide(d, s)}
| d = digit {Lettre(d)}

transitions:
  TRANSITIONS; tl = translist {Transitions(tl)}

translist:
  tl = list(transition) {Translist(tl)}

transition:
  LPAREN; l1 = digit; COMMA; l2 = lettre_min_ou_vide; COMMA; l3 = upper; COMMA; l4 = digit; COMMA; s = stack; RPAREN {Transition(l1, l2, l3, l4, s)}

lettre_min_ou_vide:
  l = lower {Some(l)}
| {None}

nonemptystack:
  l = upper; SEMI; st = nonemptystack {Nonemptystack(l, st)}
| l = upper {Lettre(l)}

stack:
  s = nonemptystack {Stack(s)}
| {Emptystack}
