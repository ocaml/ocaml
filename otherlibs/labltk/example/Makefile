include ../support/Makefile.common

COMPFLAGS=-I ../lib -I ../support 

all: hello demo eyes calc clock tetris

opt: hello.opt demo.opt eyes.opt calc.opt clock.opt tetris.opt

hello: hello.cmo
	$(LABLC) -custom $(COMPFLAGS) -o hello tk41.cma hello.cmo $(TKLINKOPT)

demo: demo.cmo
	$(LABLC) -custom $(COMPFLAGS) -o demo tk41.cma demo.cmo $(TKLINKOPT)

eyes: eyes.cmo
	$(LABLC) -custom $(COMPFLAGS) -o eyes tk41.cma eyes.cmo $(TKLINKOPT)

calc: calc.cmo
	$(LABLC) -custom $(COMPFLAGS) -o calc tk41.cma calc.cmo $(TKLINKOPT)

clock: clock.cmo
	$(LABLC) -custom $(COMPFLAGS) -o clock tk41.cma unix.cma clock.cmo \
	      $(TKLINKOPT) -cclib -lunix

clock.opt: clock.cmx
	$(CAMLOPT) $(COMPFLAGS) -o clock.opt \
	      tk41.cmxa -I $(OTHERS)/unix unix.cmxa clock.cmx \
	      $(TKLINKOPT) -cclib "-L$(OTHERS)/unix -lunix"

tetris: tetris.cmo
	$(LABLC) -custom $(COMPFLAGS) -o tetris tk41.cma tetris.cmo $(TKLINKOPT)

clean:
	rm -f hello demo eyes calc clock tetris *.opt *.o *.cm*

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .opt

.mli.cmi:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.cmx.opt:
	$(CAMLOPT) $(COMPFLAGS) -o $@ tk41.cmxa $< $(TKLINKOPT)
