include ../support/Makefile.common

COMPFLAGS=-I ../lib -I ../labltk -I ../support -I $(OTHERS)/unix -w s -dllpath ../support

all: hello demo eyes calc clock tetris lang

opt: hello.opt demo.opt eyes.opt calc.opt clock.opt tetris.opt

hello: hello.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o hello $(LIBNAME).cma hello.cmo

demo: demo.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o demo $(LIBNAME).cma demo.cmo

eyes: eyes.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o eyes $(LIBNAME).cma eyes.cmo

calc: calc.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o calc $(LIBNAME).cma calc.cmo

clock: clock.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o clock $(LIBNAME).cma unix.cma clock.cmo

clock.opt: clock.cmx
	$(CAMLOPT) -nojoin $(COMPFLAGS) -o clock.opt \
	      $(LIBNAME).cmxa unix.cmxa clock.cmx

tetris: tetris.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o tetris $(LIBNAME).cma tetris.cmo

taquin: taquin.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o taquin $(LIBNAME).cma taquin.cmo

lang: lang.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o lang $(LIBNAME).cma lang.cmo

clean:
	rm -f hello demo eyes calc clock tetris lang *.opt *.o *.cm*

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .opt

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -nojoin -c $(COMPFLAGS) $<

.cmx.opt:
	$(CAMLOPT) -nojoin $(COMPFLAGS) -o $@ $(LIBNAME).cmxa $<
