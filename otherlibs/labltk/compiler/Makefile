include ../support/Makefile.common

OBJS=	../support/support.cmo flags.cmo copyright.cmo \
	tsort.cmo tables.cmo printer.cmo lexer.cmo \
	pplex.cmo ppyac.cmo ppexec.cmo ppparse.cmo \
	parser.cmo compile.cmo intf.cmo maincompile.cmo

PPOBJS= pplex.cmo ppyac.cmo ppexec.cmo ppparse.cmo pp.cmo

all: tkcompiler$(EXE) pp$(EXE)

tkcompiler$(EXE) : $(OBJS)
	$(CAMLC) -g $(LINKFLAGS) -o tkcompiler$(EXE) $(OBJS)

pp$(EXE): $(PPOBJS)
	$(CAMLC) -g $(LINKFLAGS) -o pp$(EXE) $(PPOBJS)

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(CAMLYACC) -v parser.mly

pplex.ml: pplex.mll
	$(CAMLLEX) pplex.mll

pplex.mli: ppyac.cmi

ppyac.ml ppyac.mli: ppyac.mly
	$(CAMLYACC) -v ppyac.mly

copyright.ml: copyright
	(echo "let copyright=\"\\"; \
	cat copyright; \
	echo "\""; \
	echo "let write ~w = w copyright;;") > copyright.ml 

clean : 
	rm -f *.cm* parser.ml parser.mli lexer.ml copyright.ml
	rm -f pplex.ml ppyac.ml ppyac.mli ppyac.output
	rm -f tkcompiler$(EXE) pp$(EXE) parser.output

scratch :
	rm -f *.cm*  parser.ml parser.mli lexer.ml tkcompiler$(EXE)
	rm -f *.cm*  pplex.ml ppyac.ml ppyac.mli pp$(EXE)

install: 
	cp tkcompiler$(EXE) $(INSTALLDIR)
	cp pp$(EXE) $(INSTALLDIR)

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp

.mli.cmi:
	$(CAMLCOMP) -g $(COMPFLAGS) -I ../support $<

.ml.cmo:
	$(CAMLCOMP) -g $(COMPFLAGS) -I ../support $<

depend: parser.ml parser.mli lexer.ml pplex.ml ppyac.ml ppyac.mli
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
