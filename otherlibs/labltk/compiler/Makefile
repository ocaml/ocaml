include ../Makefile.config

OBJS=tsort.cmo tables.cmo lexer.cmo parser.cmo  compile.cmo intf.cmo \
     maincompile.cmo

tkcompiler : $(OBJS)
	$(LABLC) $(LINKFLAGS) -o tkcompiler $(OBJS)

lexer.ml: lexer.mll
	$(LABLLEX) lexer.mll

parser.ml parser.mli: parser.mly
	$(LABLYACC) -v parser.mly

clean : 
	rm -f *.cm*  parser.ml parser.mli lexer.ml tkcompiler parser.output

scratch :
	rm -f *.cm*  parser.ml parser.mli lexer.ml tkcompiler

install: 
	cp tkcompiler $(INSTALLDIR)

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .mlp

.mli.cmi:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(LABLCOMP) $(COMPFLAGS) $<

depend: parser.ml parser.mli lexer.ml
	$(LABLDEP) *.mli *.ml > .depend

include .depend
