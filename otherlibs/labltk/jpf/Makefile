include ../support/Makefile.common

COMPFLAGS=-I ../lib -I ../support -I $(OTHERS)/unix -I $(OTHERS)/str

OBJS= fileselect.cmo balloon.cmo

OBJSX = $(OBJS:.cmo=.cmx)

all: libjpf.cma

opt: libjpf.cmxa

test: balloontest

testopt: balloontest.opt

libjpf.cma: $(OBJS)
	$(LABLLIBR) -o libjpf.cma $(OBJS)

libjpf.cmxa: $(OBJSX)
	$(CAMLOPTLIBR) -o libjpf.cmxa $(OBJSX)

install: libjpf.cma
	cp $(OBJS:.cmo=.cmi) $(OBJS:.cmo=.mli) libjpf.cma $(LABLTKDIR)

installopt: libjpf.cmxa
	cp libjpf.cmxa libjpf.a $(OBJS:.cmo=.cmx) $(LABLTKDIR)

clean:
	rm -f *.cm* *.o *.a *~ *test

### Tests

balloontest: balloontest.cmo 
	$(LABLC) -o balloontest -I ../support -I ../lib \
	        -custom labltk.cma libjpf.cma balloontest.cmo

balloontest.opt: balloontest.cmx
	$(CAMLOPT) -o balloontest.opt -I ../support -I ../lib \
	        labltk.cmxa libjpf.cmxa balloontest.cmx

balloontest.cmo : balloon.cmo libjpf.cma

balloontest.cmx : balloon.cmx libjpf.cmxa

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo

.mli.cmi:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend: 
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak; \
	 $(LABLDEP) *.mli *.ml) > Makefile


### EVERYTHING THAT GOES BEYOND THIS COMMENT IS GENERATED
### DO NOT DELETE THIS LINE
balloon.cmo: balloon.cmi 
balloon.cmx: balloon.cmi 
balloontest.cmo: balloon.cmi 
balloontest.cmx: balloon.cmx 
fileselect.cmo: fileselect.cmi 
fileselect.cmx: fileselect.cmi 
