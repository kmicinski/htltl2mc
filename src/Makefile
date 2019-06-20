## Model checker makefile (just a wrapper around ocamlbuild)

PKGS = ounit,xml-light,str

all:
	ocamlbuild main.native -pkgs $(PKGS) -j 0

test:
	ocamlbuild test.native -pkgs $(PKGS) -j 0
	./test.native

.PHONY: doc

## 
## I can't find out how to make a toplevel properly with ocamlbuild
## 

MODS = atom.ml utils.ml gff.ml formulas.ml formula_parser.ml	\
     formula_lexer.ml model_lexer.ml model.ml			\
    model_parser.ml automata.ml checker.ml imp_to_lts.ml

IFACES = atom.mli utils.mli gff.mli formulas.mli formula_parser.mli	\
 model.mli model_parser.mli automata.mli checker.mli imp_to_lts.mli

.PHONY: top

allp: 
	ocamllex formula_lexer.mll
	ocamlyacc formula_parser.mly
	ocamllex model_lexer.mll
	ocamlyacc model_parser.mly
	ocamlfind ocamlc -package ocamlgraph -package xml-light $(IFACES) $(MODS) -linkpkg

top:	all
	ocamlfind ocamlmktop -o myocaml -package ocamlgraph,xml-light -linkpkg -o top `echo $(MODS) | sed 's/.ml/.cmo/'`

doc:
	ocamlbuild model-checker.docdir/index.html -pkgs $(PKGS)

clean:
	rm *.cmi *.cmo
	rm -rf doc/
