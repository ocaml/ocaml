include ../support/Makefile.common

COMPFLAGS=-I ../lib -I ../support -I $(OTHERS)/unix -w s

all: hello demo eyes calc clock tetris

opt: hello.opt demo.opt eyes.opt calc.opt clock.opt tetris.opt

hello: hello.cmo
	$(LABLC) $(COMPFLAGS) -o hello tk41.cma hello.cmo $(TKLINKOPT)

demo: demo.cmo
	$(LABLC) $(COMPFLAGS) -o demo tk41.cma demo.cmo $(TKLINKOPT)

eyes: eyes.cmo
	$(LABLC) $(COMPFLAGS) -o eyes tk41.cma eyes.cmo $(TKLINKOPT)

calc: calc.cmo
	$(LABLC) $(COMPFLAGS) -o calc tk41.cma calc.cmo $(TKLINKOPT)

clock: clock.cmo
	$(LABLC) $(COMPFLAGS) -o clock tk41.cma unix.cma clock.cmo $(TKLINKOPT)

clock.opt: clock.cmx
	$(CAMLOPT) $(COMPFLAGS) -o clock.opt \
	      tk41.cmxa unix.cmxa clock.cmx $(TKLINKOPT)

tetris: tetris.cmo
	$(LABLC) $(COMPFLAGS) -o tetris tk41.cma tetris.cmo $(TKLINKOPT)

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
