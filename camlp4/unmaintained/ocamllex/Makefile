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
# Makefile for pa_ocamllex
# M.Mauny
#

include ../../config/Makefile.cnf

OCAMLTOP=../../..

CAMLP4=../../camlp4/camlp4$(EXE)

OCAMLC=$(OCAMLTOP)/ocamlc$(EXE)
OCAMLOPT=$(OCAMLTOP)/ocamlopt$(EXE)

P4INCLUDES= -nolib -I ../../etc -I ../../meta
OCAMLINCLUDES= -nostdlib -I $(OCAMLTOP)/stdlib -I ../../camlp4 -I ../../lib -I $(OCAMLTOP)/lex
OCAMLCFLAGS=-warn-error A $(OCAMLINCLUDES)

SRC=pa_ocamllex.ml
OBJS=pa_ocamllex.cmo
OBJSX=$(OBJS:.cmo=.cmx)

all: $(OBJS) pa_ocamllex.cma

opt: $(OBJSX) pa_ocamllex.cmxa

pa_ocamllex.cma: pa_ocamllex.cmo
	$(OCAMLC) $(OCAMLCFLAGS) cset.cmo syntax.cmo table.cmo lexgen.cmo compact.cmo pa_ocamllex.cmo -a -o pa_ocamllex.cma

pa_ocamllex.cmxa: pa_ocamllex.cmo
	$(OCAMLOPT) $(OCAMLCFLAGS) cset.cmx syntax.cmx table.cmx lexgen.cmx compact.cmx pa_ocamllex.cmx -a -o pa_ocamllex.cmxa

clean:
	rm -f *.cm* *.$(O) *.$(A) *.bak .*.bak

depend:

.SUFFIXES: .cmx .cmo .cmi .ml .mli

.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) -c $<


.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) -pp "$(CAMLP4) $(P4INCLUDES) `awk 'NR == 1 { ORS=" "; for (i=2; i < NF; i++) print $$i; } ' $<`" -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLCFLAGS) -pp "$(CAMLP4) $(P4INCLUDES) `awk 'NR == 1 { ORS=" "; for (i=2; i < NF; i++) print $$i; } ' $<`" -c $<
