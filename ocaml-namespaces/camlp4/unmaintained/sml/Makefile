#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#                                Camlp4                                 #
#                                                                       #
#   Copyright 2004 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################
#
# Makefile for pa_sml
# M.Mauny
#

include ../../config/Makefile.cnf

OCAMLTOP=../../..

OCAMLC=$(OCAMLTOP)/ocamlc$(EXE) -nostdlib -I $(OCAMLTOP)/stdlib
OCAMLOPT=$(OCAMLTOP)/ocamlopt$(EXE) -nostdlib -I $(OCAMLTOP)/stdlib

P4INCLUDES=-I ../../meta -I ../../etc -I ../../lib -I ../../camlp4
OCAMLINCLUDES=-I ../../meta -I ../../lib  -I ../../camlp4

CAMLP4=camlp4$(EXE) -nolib
OCAMLCFLAGS=-warn-error A $(OCAMLINCLUDES)

SRC=pa_sml.ml
OBJS=$(SRC:.ml=.cmo)
OBJSX=$(SRC:.ml=.cmx)

all: $(OBJS) smllib.cmo

opt: $(OBJSX) smllib.cmx

depend:
	cp .depend .depend.bak
	> .depend
	for file in $(SRC); do \
	  $(CAMLP4) $(P4INCLUDES) `awk 'NR == 1 { ORS=" "; for (i=2; i < NF; i++) print $$i; } ' $$file` pr_depend.cmo $$i | \
	  sed -e 's| \.\./\.\./\.\.| $$(OCAMLTOP)|g' >> .depend; \
	done

clean:
	rm -f *.cm* *.o *.bak .*.bak


.SUFFIXES: .cmx .cmo .cmi .ml .mli .sml

.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) -c $<


.sml.cmo:
	$(OCAMLC) -I ../../../otherlibs/unix -pp "$(CAMLP4) ./pa_sml.cmo ../../meta/pr_dump.cmo -impl" -c -o $*.cmo -impl $<

.sml.cmx:
	$(OCAMLOPT)  -I ../../../otherlibs/unix -pp "$(CAMLP4) ./pa_sml.cmo ../../meta/pr_dump.cmo -impl" -c -o $*.cmx -impl $<

.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) -pp "$(CAMLP4)  $(P4INCLUDES) `awk 'NR == 1 { ORS=" "; for (i=2; i < NF; i++) print $$i; } ' $<`" -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLCFLAGS) -pp "$(CAMLP4)  $(P4INCLUDES) `awk 'NR == 1 { ORS=" "; for (i=2; i < NF; i++) print $$i; } ' $<`" -c $<

include .depend
