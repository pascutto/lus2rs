CMO=lexer_lustre.cmo parser_lustre.cmo ident.cmo printer_ast.cmo sugar.cmo typer.cmo printer_typed_ast.cmo initializer.cmo clocker.cmo printer_clocked_ast.cmo normalizer.cmo scheduler.cmo translater.cmo printer_obj_ast.cmo lus2rs.cmo
GENERATED = lexer_lustre.ml parser_lustre.ml parser_lustre.mli
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
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED) parser_lustre.automaton parser_lustre.conflicts .depend

parser_lustre.ml: ast_types_lustre.cmi ast_lustre.cmi

typer.ml: ast_typed_lustre.cmi

clocker.ml: ast_clocked_lustre.cmi

translater.ml: ast_obj.cmi

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
