# $Id$

FILES=config.cmi warnings.cmi config.cmo
INCL=

all: $(FILES)

clean:
	rm -f *.cm[oi]

depend:
	ocamldep $(INCL) *.ml* > .depend

.SUFFIXES: .mli .cmi .ml .cmo

.mli.cmi:
	ocamlc $(INCL) -c $<

.ml.cmo:
	ocamlc $(INCL) -c $<

include .depend
