# $Id$

# Change the value of PROFILING to prof for systematically building
# and installing profiled versions of Camlp4 libraries. Then, execute
# `make opt.opt', then `make install' in the OCaml toplevel directory
# (or in the camlp4 subdirectory).

# Default value is noprof

#PROFILING=prof
PROFILING=noprof

###########################################################################

CAMLP4_COMM=OTOP=$(OTOP) OPT=$(OPT) EXE=$(EXE) ../tools/camlp4_comm.sh
OCAMLC=@OTOP=$(OTOP) OPT=$(OPT) EXE=$(EXE) ../tools/ocamlc.sh
OCAMLOPT=@OTOP=$(OTOP) OPT=$(OPT) EXE=$(EXE) ../tools/ocamlopt.sh
OCAMLCFLAGS=
MKDIR=mkdir -p

TEST_DIRECTORY=test `basename "$<"` = "$<" || { echo "You are not in the right directory"; exit 1; }

.SUFFIXES: .cmx .cmo .cmi .ml .mli .p.cmx

.mli.cmi:
	@$(TEST_DIRECTORY)
	@$(CAMLP4_COMM) $< -o $*.ppi
	$(OCAMLC) $(OCAMLCFLAGS) -c -intf $*.ppi
	rm -f $*.ppi	

.ml.cmo:
	@$(TEST_DIRECTORY)
	@$(CAMLP4_COMM) $< -o $*.ppo
	$(OCAMLC) $(OCAMLCFLAGS) -c -impl $*.ppo
	rm -f $*.ppo

.ml.cmx:
	@$(TEST_DIRECTORY)
	@$(CAMLP4_COMM) $< -o $*.ppo
	$(OCAMLOPT) $(OCAMLCFLAGS) -c -impl $*.ppo
	rm -f $*.ppo

.ml.p.cmx:
	@$(TEST_DIRECTORY)
	@$(CAMLP4_COMM) $< -o $*.ppo
	$(OCAMLOPT) $(OCAMLCFLAGS) -c -p -o $*.p.cmx -impl $*.ppo
	rm -f $*.ppo

