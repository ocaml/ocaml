# $Id$

EXE=
CAMLP4_COMM=OTOP=$(OTOP) OPT=$(OPT) EXE=$(EXE) ../tools/camlp4_comm.sh
OCAMLC=@OTOP=$(OTOP) OPT=$(OPT) ../tools/ocamlc.sh
OCAMLOPT=@OTOP=$(OTOP) OPT=$(OPT) ../tools/ocamlopt.sh
OCAMLCFLAGS=
MKDIR=mkdir -p

.SUFFIXES: .cmx .cmo .cmi .ml .mli

.mli.cmi:
	@if test `basename $<` != $<; then echo "Bad directory"; exit 1; fi
	@$(CAMLP4_COMM) $< -o $*.ppi
	$(OCAMLC) $(OCAMLCFLAGS) -c -intf $*.ppi
	rm -f $*.ppi	

.ml.cmo:
	@if test `basename $<` != $<; then echo "Bad directory"; exit 1; fi
	@$(CAMLP4_COMM) $< -o $*.ppo
	$(OCAMLC) $(OCAMLCFLAGS) -c -impl $*.ppo
	rm -f $*.ppo

.ml.cmx:
	@if test `basename $<` != $<; then echo "Bad directory"; exit 1; fi
	@$(CAMLP4_COMM) $< -o $*.ppo
	$(OCAMLOPT) $(OCAMLCFLAGS) -c -impl $*.ppo
	rm -f $*.ppo
