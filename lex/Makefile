# The lexer generator

CAMLC=../boot/camlrun ../boot/camlc -I ../boot
COMPFLAGS=
LINKFLAGS=
CAMLYACC=../boot/camlyacc
YACCFLAGS=
CAMLLEX=../boot/camlrun ../boot/camllex
CAMLDEP=../tools/camldep
DEPFLAGS=

OBJS=syntax.cmo parser.cmo lexer.cmo lexgen.cmo output.cmo main.cmo

all: camllex

camllex: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o camllex $(OBJS)

clean::
	rm -f camllex
	rm -f *.cmo *.cmi camllex

parser.ml parser.mli: parser.mly
	$(CAMLYACC) $(YACCFLAGS) parser.mly

clean::
	rm -f parser.ml parser.mli

beforedepend:: parser.ml parser.mli

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

clean::
	rm -f lexer.ml

beforedepend:: lexer.ml

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
