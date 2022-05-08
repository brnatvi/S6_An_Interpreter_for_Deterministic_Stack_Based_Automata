MENHIR=menhir
OCAMLC=ocamlc
OCAMLLEX=ocamllex

SOURCES = ast.ml prints.ml interpret.ml grammaire.ml lexer.ml main.ml 

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

lexer.mll: grammaire.ml

clean:
	rm -fr grammaire grammaire.mli grammaire.ml lexer.ml *.cmo *.cmi *.automaton *.conflicts

grammaire.cmo: ast.cmo grammaire.cmi
prints.cmo: grammaire.cmo
interpret.cmo: grammaire.cmo

lexer.cmo: grammaire.cmo
main.cmo: grammaire.cmo lexer.cmo interpret.cmo prints.cmo