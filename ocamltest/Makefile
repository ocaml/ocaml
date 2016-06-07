modules :=\
  run.o \
  run_stubs.o \
  run.cmo \
  testlib.cmo \
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

bcflags = -g -nostdlib -I ../parsing -I ../stdlib -I ../compilerlibs

ocamlc := ../byterun/ocamlrun ../ocamlc $(bcflags)

ocamltest.byte: $(modules)
	$(ocamlc) -custom ocamlcommon.cma -o $@ $^

%.cmo: %.ml %.cmi
	$(ocamlc) -c $<

main.cmo: main.ml
	$(ocamlc) -c $<

%.cmi: %.mli
	$(ocamlc) -c $<

%.ml %.mli: %.mly
	ocamlyacc $<

%.ml: %.mll
	ocamllex -q $<

.PHONY: clean

clean:
	rm -rf *.cm* *.o ocamltest.byte
	rm -rf tsl_parser.ml tsl_parser.mli tsl_lexer.ml


