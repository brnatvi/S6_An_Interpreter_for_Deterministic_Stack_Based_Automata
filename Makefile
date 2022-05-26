MENHIR=menhir
OCAMLC=ocamlc
OCAMLLEX=ocamllex

SOURCES = ast.ml ast2.ml prints.ml prints2.ml interpret.ml interpret2.ml grammaire.ml grammaire2.ml lexer.ml lexer2.ml main.ml 

OBJECTS = $(SOURCES:.ml=.cmo)

.PHONY: clean all 

all: grammaire

grammaire: $(OBJECTS)
	$(OCAMLC) -o $@ $(OBJECTS)

%.cmo: %.ml
	$(OCAMLC) -c $< -o $@

%.cmi: %.mli
	$(OCAMLC) -c $< -o $@

%.ml %.mli: %.mly
	rm -f $(<:.mly=.conflicts)
	$(MENHIR) -v --infer $<

%.ml: %.mll
	$(OCAMLLEX) $<

grammaire.mly: ast.ml
grammaire2.mly: ast2.ml

lexer.mll: grammaire.ml
lexer2.mll: grammaire2.ml

clean:
	rm -fr grammaire grammaire.mli grammaire.ml grammaire2.mli grammaire2.ml lexer.ml lexer2.ml *.cmo *.cmi *.automaton *.conflicts

grammaire.cmo: ast.cmo grammaire.cmi
grammaire2.cmo: ast2.cmo grammaire2.cmi

prints.cmo: grammaire.cmo
prints2.cmo: grammaire2.cmo

interpret.cmo: grammaire.cmo
interpret2.cmo: grammaire2.cmo

lexer.cmo: grammaire.cmo
lexer2.cmo: grammaire2.cmo

main.cmo: grammaire.cmo grammaire2.cmo lexer.cmo lexer2.cmo interpret.cmo interpret2.cmo prints.cmo prints2.cmo