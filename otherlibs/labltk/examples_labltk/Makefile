include ../support/Makefile.common

COMPFLAGS=-I ../lib -I ../labltk -I ../support -I $(OTHERS)/unix -w s -dllpath ../support

all: hello demo eyes calc clock tetris

opt: hello.opt demo.opt eyes.opt calc.opt clock.opt tetris.opt

hello: hello.cmo
	$(LABLC) $(COMPFLAGS) -o hello labltk.cma hello.cmo

demo: demo.cmo
	$(LABLC) $(COMPFLAGS) -o demo labltk.cma demo.cmo

eyes: eyes.cmo
	$(LABLC) $(COMPFLAGS) -o eyes labltk.cma eyes.cmo

calc: calc.cmo
	$(LABLC) $(COMPFLAGS) -o calc labltk.cma calc.cmo

clock: clock.cmo
	$(LABLC) $(COMPFLAGS) -o clock labltk.cma unix.cma clock.cmo

clock.opt: clock.cmx
	$(CAMLOPT) $(COMPFLAGS) -o clock.opt \
	      labltk.cmxa unix.cmxa clock.cmx

tetris: tetris.cmo
	$(LABLC) $(COMPFLAGS) -o tetris labltk.cma tetris.cmo

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
	$(CAMLOPT) $(COMPFLAGS) -o $@ labltk.cmxa $<
