MENHIR=menhir
OCAMLC=ocamlc
OCAMLLEX=ocamllex

SOURCES = ast.ml ast2.ml prints.ml interpret.ml interpret2.ml grammaire.ml grammaire2.ml lexer.ml lexer2.ml main.ml 

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

grammaire2.mly: ast2.ml

lexer2.mll: grammaire2.ml

clean:
	rm -fr grammaire grammaire.mli grammaire.ml grammaire2.mli grammaire2.ml lexer.ml *.cmo *.cmi *.automaton *.conflicts

grammaire.cmo: ast.cmo grammaire.cmi
grammaire2.cmo: ast2.cmo grammaire2.cmi

prints.cmo: grammaire.cmo
interpret.cmo: grammaire.cmo
interpret2.cmo: grammaire2.cmo

lexer.cmo: grammaire.cmo
lexer2.cmo: grammaire2.cmo
main.cmo: grammaire.cmo lexer.cmo interpret.cmo prints.cmo