include ../support/Makefile.common

COMPFLAGS=-I ../labltk -I ../support -I $(OTHERS)/unix -I $(OTHERS)/str

OBJS= fileselect.cmo balloon.cmo shell.cmo jpf_font.cmo

OBJSX = $(OBJS:.cmo=.cmx)

all: libjpf.cma

opt: libjpf.cmxa

test: balloontest

testopt: balloontest.opt

libjpf.cma: $(OBJS)
	$(CAMLLIBR) -o libjpf.cma $(OBJS)

libjpf.cmxa: $(OBJSX)
	$(CAMLOPTLIBR) -o libjpf.cmxa $(OBJSX)

install: libjpf.cma
	cp $(OBJS:.cmo=.cmi) $(OBJS:.cmo=.mli) libjpf.cma $(INSTALLDIR)

installopt: libjpf.cmxa
	cp libjpf.cmxa libjpf.a $(OBJS:.cmo=.cmx) $(INSTALLDIR)

clean:
	rm -f *.cm* *.o *.a *~ *test

$(OBJS) $(OBJS:.cmo=.cmi): ../lib/$(LIBNAME).cma

$(OBJSX): ../lib/$(LIBNAME).cmxa

### Tests

balloontest: balloontest.cmo 
	$(CAMLC) -o balloontest -I ../support -I ../lib \
	        -custom $(LIBNAME).cma libjpf.cma balloontest.cmo

balloontest.opt: balloontest.cmx
	$(CAMLOPT) -o balloontest.opt -I ../support -I ../lib \
	        $(LIBNAME).cmxa libjpf.cmxa balloontest.cmx

balloontest.cmo : balloon.cmo libjpf.cma

balloontest.cmx : balloon.cmx libjpf.cmxa

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend: 
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 $(CAMLDEP) *.mli *.ml) > Makefile


### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
balloon.cmo: balloon.cmi 
balloon.cmx: balloon.cmi 
fileselect.cmo: fileselect.cmi 
fileselect.cmx: fileselect.cmi 
jpf_font.cmo: shell.cmi jpf_font.cmi 
jpf_font.cmx: shell.cmx jpf_font.cmi 
shell.cmo: shell.cmi 
shell.cmx: shell.cmi 
