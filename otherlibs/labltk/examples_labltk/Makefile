include ../support/Makefile.common

COMPFLAGS=-I ../lib -I ../labltk -I ../support -I $(OTHERS)/unix -w s -dllpath ../support

all: hello demo eyes calc clock tetris lang

opt: hello.opt demo.opt eyes.opt calc.opt clock.opt tetris.opt

hello: hello.cmo
	$(CAMLC) $(COMPFLAGS) -o hello labltk.cma hello.cmo

demo: demo.cmo
	$(CAMLC) $(COMPFLAGS) -o demo labltk.cma demo.cmo

eyes: eyes.cmo
	$(CAMLC) $(COMPFLAGS) -o eyes labltk.cma eyes.cmo

calc: calc.cmo
	$(CAMLC) $(COMPFLAGS) -o calc labltk.cma calc.cmo

clock: clock.cmo
	$(CAMLC) $(COMPFLAGS) -o clock labltk.cma unix.cma clock.cmo

clock.opt: clock.cmx
	$(CAMLOPT) $(COMPFLAGS) -o clock.opt \
	      labltk.cmxa unix.cmxa clock.cmx

tetris: tetris.cmo
	$(CAMLC) $(COMPFLAGS) -o tetris labltk.cma tetris.cmo

lang: lang.cmo
	$(CAMLC) $(COMPFLAGS) -o lang labltk.cma lang.cmo

clean:
	rm -f hello demo eyes calc clock tetris lang *.opt *.o *.cm*

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .opt

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.cmx.opt:
	$(CAMLOPT) $(COMPFLAGS) -o $@ labltk.cmxa $<
