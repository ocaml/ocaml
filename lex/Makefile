# The lexer generator

CAMLC=../boot/cslrun ../boot/cslc -I ../boot
COMPFLAGS=
LINKFLAGS=
CAMLYACC=../boot/cslyacc
YACCFLAGS=
CAMLLEX=../boot/cslrun ../boot/csllex
CAMLDEP=../boot/cslrun ../tools/csldep
DEPFLAGS=

OBJS=parser.cmo lexer.cmo lexgen.cmo compact.cmo output.cmo main.cmo

all: csllex

csllex: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o csllex $(OBJS)

clean::
	rm -f csllex
	rm -f *.cmo *.cmi

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
