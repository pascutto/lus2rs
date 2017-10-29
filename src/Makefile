CMO=lustre_lexer.cmo lustre_parser.cmo ident.cmo typed_ast_printer.cmo typer.cmo normalizer.cmo scheduler.cmo lus2rs.cmo
GENERATED = lustre_lexer.ml lustre_parser.ml lustre_parser.mli
BIN=lus2rs
FLAGS=

all: $(BIN)

$(BIN): $(CMX)

$(BIN): $(CMO)
	ocamlc $(FLAGS) -o $(BIN) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c  $<

.mll.ml:
	ocamllex -q $<

.mly.ml:
	menhir --infer -v $<

clean:
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED) lustre_parser.automaton lustre_parser.conflicts .depend

lustre_parser.ml: lustre_ast_types.cmi lustre_ast.cmi

typer.ml: lustre_typed_ast.cmi

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
