ROOT=../..
CAMLP4LIB=$(ROOT)/camlp4/meta
OCAMLLIB=$(ROOT)/stdlib

OCAMLRUN = $(ROOT)/boot/ocamlrun
CAMLP4O = CAMLP4LIB=$(CAMLP4LIB) $(OCAMLRUN) $(ROOT)/camlp4/etc/camlp4o
OCAMLC = OCAMLLIB=$(OCAMLLIB) $(OCAMLRUN) $(ROOT)/ocamlc
OCAMLCFLAGS = -I $(ROOT)/otherlibs/dyntypes
OCAMLDEP = $(OCAMLRUN) $(ROOT)/tools/ocamldep
JOCPARSER = $(ROOT)/jocparsing/pa_joc.cmo

tests = basic

default: build run

build: p4_dyntest.cmo basic.byte

run: $(tests:=.byte)
	@set -e; \
	for x in $(tests); do \
	  echo "[$$x]"; \
	  $(OCAMLRUN) ./$$x.byte; \
	done; \
	echo "All tests succeeded."

.SUFFIXES: .mli .ml .cmi .cmo .cmx .cma .cmxa .byte .opt

.ml.cmo:
	$(OCAMLC) -join $(OCAMLCFLAGS) -pp "$(CAMLP4O) ./p4_dyntest.cmo $(JOCPARSER)" -c $<

.cmo.byte:
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ dynamics.cma $<

p4_dyntest.cmo: p4_dyntest.ml
	$(OCAMLC) $(OCAMLCFLAGS) -I $(CAMLP4LIB) -I $(ROOT)/camlp4/lib -I $(ROOT)/camlp4/camlp4 -o $@ -pp "$(CAMLP4O) q_MLast.cmo pa_extend.cmo" -c p4_dyntest.ml

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.cmxa *.o *.byte *.opt

depend: p4_dyntest.cmo
	$(OCAMLDEP) -pp "$(CAMLP4O) ./p4_dyntest.cmo" $(tests:=.ml) >.depend
	set -e; \
	for x in $(tests); do \
	  echo "$$x.cmo: p4_dyntest.cmo ../../otherlibs/dyntypes/dynamics.cma"; \
	done >>.depend

include .depend
