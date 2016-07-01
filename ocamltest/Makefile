CFLAGS = -I ../stdlib -g -O0
modules :=\
  run.o \
  run_stubs.o \
  run.cmo \
  testlib.cmo \
  filetype.cmo \
  filecompare.cmo \
  backends.cmo \
  environments.cmo \
  builtin_environments.cmo \
  actions.cmo \
  builtin_actions.cmo \
  tests.cmo \
  builtin_tests.cmo \
  tsl_ast.cmo \
  tsl_parser.cmo \
  tsl_lexer.cmo \
  tsl_semantics.cmo \
  main.cmo

optmodules := $(modules:.cmo=.cmx)

flags = -g -nostdlib -I ../parsing -I ../stdlib -I ../compilerlibs \
  -strict-sequence -safe-string -strict-formats \
  -w +a-4-9-41-42-44-45-48 -warn-error A

ocamlc := ../byterun/ocamlrun ../ocamlc $(flags)

ocamlopt := ../byterun/ocamlrun ../ocamlopt $(flags)

ocamltest.byte: $(modules)
	$(ocamlc) -custom ocamlcommon.cma -o $@ $^

%.cmo: %.ml %.cmi
	$(ocamlc) -c $<

ocamltest.opt: $(optmodules)
	$(ocamlopt) ocamlcommon.cmxa -o $@ $^



%.cmx: %.ml %.cmi
	$(ocamlopt) -c $<

main.cmo: main.ml
	$(ocamlc) -c $<

main.cmx: main.ml
	$(ocamlopt) -c $<

%.cmi: %.mli
	$(ocamlc) -c $<

%.ml %.mli: %.mly
	ocamlyacc $<

%.ml: %.mll
	ocamllex -q $<

.PHONY: clean

clean:
	rm -rf *.cm* *.o ocamltest.byte ocamltest.opt
	rm -rf tsl_parser.ml tsl_parser.mli tsl_lexer.ml


