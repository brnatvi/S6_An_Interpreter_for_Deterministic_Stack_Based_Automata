%{
open Ast2
%}

%token COMMA COLON EOF BEGIN END POP PUSH CHANGE REJECT STATE TOP NEXT CASE OF
%token INPUTSYMBOLS STACKSYMBOLS STATES INITIALSTATE INITIALSTACK PROGRAM
%token <char> UPPER LOWER
%token <int> DIGIT
%start <Ast2.automate> automate

%%

automate:
  d = declarations; ins = first_instruction; EOF {Automate(d, ins)}

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

case_state:
  d = digit; COLON; i = instruction {Case(d, i)}
case_next:
  c = lower; COLON; i = instruction {Case(c, i)}
case_top:
  c = upper; COLON; i = instruction {Case(c, i)}

switch_case_state:
  CASE; STATE; OF; li = list(case_state) {SwitchCaseState(li)}

switch_case:
  CASE; NEXT; OF; li = list(case_next) {SwitchCaseNext(li)}
| CASE; TOP; OF; li = list(case_top) {SwitchCaseNext(li)}
  
instruction:
  POP; CHANGE; d = digit {PopAndChange(d)}
| CHANGE; d = digit; POP {PopAndChange(d)}
| PUSH; c = upper; CHANGE; d = digit {PushAndChange(c,d)}
| CHANGE; d = digit; PUSH; c = upper {PushAndChange(c,d)}
| POP {Pop}
| PUSH; c = upper {Push(c)}
| REJECT {Reject}
| CHANGE; d = digit {Change(d)}
| BEGIN; sc = switch_case; END {SwitchCase(sc)}

first_instruction:
  PROGRAM; s = switch_case_state {SwitchCase(s)}


