CAMLC=../boot/ocamlrun ../ocamlc -I ../stdlib
COMPFLAGS=$(INCLUDES)
LINKFLAGS=-custom
CAMLYACC=../boot/ocamlyacc
YACCFLAGS=
CAMLLEX=../boot/ocamlrun ../boot/ocamllex
CAMLDEP=../boot/ocamlrun ../tools/ocamldep
DEPFLAGS=$(INCLUDES)

INCLUDES=-I ../otherlibs/unix -I ../utils -I ../parsing -I ../typing -I ../bytecomp

OTHEROBJS=\
  ../otherlibs/unix/unix.cma \
  ../utils/misc.cmo ../utils/config.cmo \
  ../utils/tbl.cmo ../utils/clflags.cmo \
  ../parsing/longident.cmo \
  ../typing/ident.cmo ../typing/path.cmo ../typing/types.cmo \
  ../typing/primitive.cmo ../typing/typedtree.cmo \
  ../typing/subst.cmo ../typing/predef.cmo \
  ../typing/datarepr.cmo ../typing/env.cmo \
  ../typing/ctype.cmo ../typing/printtyp.cmo ../typing/mtype.cmo \
  ../bytecomp/runtimedef.cmo ../bytecomp/symtable.cmo

OBJS=\
	primitives.cmo \
	unix_tools.cmo \
	debugger_config.cmo \
	envaux.cmo \
	parameters.cmo \
	lexer.cmo \
	debugcom.cmo \
	exec.cmo \
	input_handling.cmo \
	source.cmo \
	checkpoints.cmo \
	symbols.cmo \
	events.cmo \
	breakpoints.cmo \
	trap_barrier.cmo \
	history.cmo \
	program_loading.cmo \
	printval.cmo \
	eval.cmo \
	show_source.cmo \
	time_travel.cmo \
	program_management.cmo \
	frames.cmo \
	show_information.cmo \
	parser.cmo \
	command_line_interpreter.cmo \
	main.cmo

all: ocamldebug

ocamldebug: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o ocamldebug $(OTHEROBJS) $(OBJS) ../otherlibs/unix/libunix.a

clean::
	rm -f ocamldebug
	rm -f *.cmo *.cmi

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) $(DEPFLAGS) *.mli *.ml > .depend

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll
clean::
	rm -f lexer.ml
beforedepend:: lexer.ml

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly
clean::
	rm -f parser.ml parser.mli
beforedepend:: parser.ml parser.mli

include .depend
