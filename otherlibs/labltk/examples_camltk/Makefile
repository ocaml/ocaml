include ../support/Makefile.common

# We are using the non-installed library !
COMPFLAGS=-I ../lib -I ../camltk -I ../support -I $(OTHERS)/unix -w s -dllpath ../support


all: addition$(EXE) helloworld$(EXE) winskel$(EXE) fileinput$(EXE) \
	eyes$(EXE) tetris$(EXE) mytext$(EXE) fileopen$(EXE)

addition$(EXE): addition.cmo
	$(LABLC) $(COMPFLAGS) -o $@ labltk.cma addition.cmo 

helloworld$(EXE): helloworld.cmo
	$(LABLC) $(COMPFLAGS) -o $@ labltk.cma helloworld.cmo 

winskel$(EXE): winskel.cmo
	$(LABLC) $(COMPFLAGS) -o $@ labltk.cma winskel.cmo

fileinput$(EXE): fileinput.cmo
	$(LABLC) $(COMPFLAGS) -o $@ unix.cma labltk.cma fileinput.cmo

socketinput$(EXE): socketinput.cmo
	$(LABLC) $(COMPFLAGS) -o $@ unix.cma labltk.cma socketinput.cmo

eyes$(EXE): eyes.cmo
	$(LABLC) $(COMPFLAGS) -o $@ unix.cma labltk.cma eyes.cmo

tetris$(EXE): tetris.cmo
	$(LABLC) $(COMPFLAGS) -o $@ unix.cma labltk.cma tetris.cmo

mytext$(EXE): mytext.cmo
	$(LABLC) $(COMPFLAGS) -o $@ unix.cma labltk.cma mytext.cmo

# graph$(EXE): graphics.cmo graphics_test.cmo
# 	  $(LABLC) -o $@ graphics.cmo graphics_test.cmo
# 
# graphics_test.cmo: graphics.cmo

fileopen$(EXE): fileopen.cmo
	$(LABLC) $(COMPFLAGS) -o $@ labltk.cma fileopen.cmo

clean : 
	rm -f *.cm? $(EXECS) addition eyes fileinput fileopen helloworld jptest mytext tetris winskel

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo

.mli.cmi:
	$(LABLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(LABLCOMP) $(COMPFLAGS) $<
