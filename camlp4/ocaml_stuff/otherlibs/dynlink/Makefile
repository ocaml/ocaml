# $Id$

EXE=
FILES=extract_crc$(EXE)
INCL=

all: $(FILES)

extract_crc$(EXE): extract_crc.cmo
	ocamlc dynlink.cma extract_crc.cmo -o extract_crc$(EXE)

clean:
	rm -f *.cm[oi] *.o extract_crc$(EXE)

depend:
	ocamldep $(INCL) *.ml* > .depend

.SUFFIXES: .ml .cmo

.ml.cmo:
	ocamlc $(INCL) -c $<

include .depend
