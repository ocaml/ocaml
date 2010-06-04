include ../support/Makefile.common

# We are using the non-installed library !
COMPFLAGS=-I ../lib -I ../camltk -I ../support -I $(OTHERS)/unix -w s -dllpath ../support


all: addition$(EXE) helloworld$(EXE) winskel$(EXE) fileinput$(EXE) \
	eyes$(EXE) tetris$(EXE) mytext$(EXE) fileopen$(EXE)

addition$(EXE): addition.cmo
<<<<<<< .courant
	$(CAMLC) -nojoin $(COMPFLAGS) -o $@ $(LIBNAME).cma addition.cmo 
=======
	$(CAMLC) $(COMPFLAGS) -o $@ $(LIBNAME).cma addition.cmo
>>>>>>> .fusion-droit.r10497

helloworld$(EXE): helloworld.cmo
<<<<<<< .courant
	$(CAMLC) -nojoin $(COMPFLAGS) -o $@ $(LIBNAME).cma helloworld.cmo 
=======
	$(CAMLC) $(COMPFLAGS) -o $@ $(LIBNAME).cma helloworld.cmo
>>>>>>> .fusion-droit.r10497

winskel$(EXE): winskel.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o $@ $(LIBNAME).cma winskel.cmo

fileinput$(EXE): fileinput.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o $@ unix.cma $(LIBNAME).cma fileinput.cmo

socketinput$(EXE): socketinput.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o $@ unix.cma $(LIBNAME).cma socketinput.cmo

eyes$(EXE): eyes.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o $@ unix.cma $(LIBNAME).cma eyes.cmo

tetris$(EXE): tetris.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o $@ unix.cma $(LIBNAME).cma tetris.cmo

mytext$(EXE): mytext.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o $@ unix.cma $(LIBNAME).cma mytext.cmo

# graph$(EXE): graphics.cmo graphics_test.cmo
# 	  $(CAMLC) -nojoin -o $@ graphics.cmo graphics_test.cmo
#
# graphics_test.cmo: graphics.cmo

fileopen$(EXE): fileopen.cmo
	$(CAMLC) -nojoin $(COMPFLAGS) -o $@ $(LIBNAME).cma fileopen.cmo

clean :
	rm -f *.cm? $(EXECS) addition eyes fileinput fileopen helloworld jptest mytext tetris winskel

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<
