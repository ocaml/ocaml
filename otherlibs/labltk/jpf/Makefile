include ../support/Makefile.common

COMPFLAGS=-I ../labltk -I ../support -I $(OTHERS)/win32unix -I $(OTHERS)/unix -I $(OTHERS)/str

OBJS= fileselect.cmo balloon.cmo shell.cmo jpf_font.cmo

OBJSX = $(OBJS:.cmo=.cmx)

all: jpflib.cma

opt: jpflib.cmxa

test: balloontest

testopt: balloontest.opt

jpflib.cma: $(OBJS)
	$(CAMLLIBR) -o jpflib.cma $(OBJS)

jpflib.cmxa: $(OBJSX)
	$(CAMLOPTLIBR) -o jpflib.cmxa $(OBJSX)

install:
	cp $(OBJS:.cmo=.cmi) $(OBJS:.cmo=.mli) jpflib.cma $(INSTALLDIR)

installopt:
	cp jpflib.cmxa jpflib.$(A) $(OBJS:.cmo=.cmx) $(INSTALLDIR)

clean:
	rm -f *.cm* *.$(O) *.$(A) *~ *test

$(OBJS) $(OBJS:.cmo=.cmi): ../lib/$(LIBNAME).cma

$(OBJSX): ../lib/$(LIBNAME).cmxa

### Tests

balloontest: balloontest.cmo 
	$(CAMLC) -o balloontest -I ../support -I ../lib \
	        -custom $(LIBNAME).cma jpflib.cma balloontest.cmo

balloontest.opt: balloontest.cmx
	$(CAMLOPT) -o balloontest.opt -I ../support -I ../lib \
	        $(LIBNAME).cmxa jpflib.cmxa balloontest.cmx

balloontest.cmo : balloon.cmo jpflib.cma

balloontest.cmx : balloon.cmx jpflib.cmxa

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
