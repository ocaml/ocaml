include ../Makefile.config

COMPFLAGS=-I ../lib -I ../support 

TKLINKOPT= -ccopt -L../support -cclib -llabltk41 $(TKLIBS) $(X11_LIBS)

all: hello demo eyes calc clock tetris

opt: hello.opt demo.opt eyes.opt calc.opt tetris.opt

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
	labltkopt $(COMPFLAGS) -o $@ $<
