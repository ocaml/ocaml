include ../support/Makefile.common

OBJS=	../support/support.cmo flags.cmo copyright.cmo \
	tsort.cmo tables.cmo printer.cmo lexer.cmo \
	pplex.cmo ppyac.cmo ppexec.cmo ppparse.cmo \
	parser.cmo compile.cmo intf.cmo maincompile.cmo

PPOBJS= pplex.cmo ppyac.cmo ppexec.cmo ppparse.cmo pp.cmo

all: tkcompiler$(EXE) pp$(EXE)

tkcompiler$(EXE) : $(OBJS)
	$(LABLC) -g $(LINKFLAGS) -o tkcompiler$(EXE) $(OBJS)

pp$(EXE): $(PPOBJS)
	$(LABLC) -g $(LINKFLAGS) -o pp$(EXE) $(PPOBJS)

lexer.ml: lexer.mll
	$(LABLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(LABLYACC) -v parser.mly

pplex.ml: pplex.mll
	$(LABLLEX) pplex.mll

ppyac.ml ppyac.mli: ppyac.mly
	$(LABLYACC) -v ppyac.mly

copyright.ml: copyright
	(echo "let copyright=\"\\"; \
	cat copyright; \
	echo "\""; \
	echo "let write ~w = w copyright;;") > copyright.ml 

clean : 
	rm -f *.cm* parser.ml parser.mli lexer.ml copyright.ml
	rm -f pplex.ml ppyac.ml ppyac.mli
	rm -f tkcompiler$(EXE) pp$(EXE) parser.output

scratch :
	rm -f *.cm*  parser.ml parser.mli lexer.ml tkcompiler$(EXE)
	rm -f *.cm*  pplex.ml ppyac.ml ppyac.mli pp$(EXE)

install: 
	cp tkcompiler$(EXE) $(LABLTKDIR)
	cp pp$(EXE) $(LABLTKDIR)

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp

.mli.cmi:
	$(LABLCOMP) -g $(COMPFLAGS) -I ../support $<

.ml.cmo:
	$(LABLCOMP) -g $(COMPFLAGS) -I ../support $<

depend: parser.ml parser.mli lexer.ml pplex.ml ppyac.ml ppyac.mli
	$(LABLDEP) *.mli *.ml > .depend

include .depend
