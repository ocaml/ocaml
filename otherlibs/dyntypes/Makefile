#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../../LICENSE.   #
#                                                                       #
#########################################################################

# $Id$

# Makefile for the dynamic types library

include ../../config/Makefile

# Compilation options
ROOT = ../..
CAMLRUN = $(ROOT)/boot/ocamlrun
JOCPARSER = $(CAMLRUN) $(ROOT)/jocparsing/jocp
CAMLC = $(CAMLRUN) $(ROOT)/ocamlc -pp "$(JOCPARSER)" -I $(ROOT)/stdlib
CAMLOPT = $(CAMLRUN) $(ROOT)/ocamlopt -pp "$(JOCPARSER)" -I $(ROOT)/stdlib
MKLIB = $(CAMLRUN) $(ROOT)/tools/ocamlmklib

LOCAL_MODULES = dynamics
UTILS_MODULES = config misc tbl clflags
PARSING_MODULES = asttypes longident
TYPING_MODULES = outcometree ident path primitive types typedynt btype subst predef datarepr env ctype printtyp
IMPORTED_MODULES = $(UTILS_MODULES) $(PARSING_MODULES) $(TYPING_MODULES)
CAMLOBJS = dyntypes_compiler_bits.cmo $(LOCAL_MODULES:=.cmo)
CMIFILES = $(CAMLOBJS:.cmo=.cmi)

default: all

dyntypes_compiler_bits.ml: gather_compiler_bits
	$(CAMLRUN) ./gather_compiler_bits $(ROOT)/utils $(ROOT)/parsing $(ROOT)/typing - $(IMPORTED_MODULES) >$@

import:
	rm -f dyntypes_compiler_bits.ml
	$(MAKE) dyntypes_compiler_bits.ml

all: dynamics.cma $(CMIFILES)

allopt: dynamics.cmxa $(CMIFILES)

dynamics.cma: $(CAMLOBJS)
	$(CAMLC) -a -o $@ $(CAMLOBJS)

dynamics.cmxa: $(CAMLOBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(CAMLOBJS:.cmo=.cmx)

$(CAMLOBJS:.cmo=.cmx): $(ROOT)/ocamlopt

install:
	cp dynamics.cma $(CMIFILES) $(LOCAL_MODULES:=.mli) $(LIBDIR)

installopt:
	cp $(CAMLOBJS:.cmo=.cmx) dynamics.cmxa $(LIBDIR)

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f dyntypes_compiler_bits.ml gather_compiler_bits
	rm -f *.a *.o *.so

.SUFFIXES: .ml .mli .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

gather_compiler_bits: gather_compiler_bits.ml
	$(CAMLC) -o $@ gather_compiler_bits.ml

depend:
	: >.depend
	$(CAMLRUN) $(ROOT)/tools/ocamldep *.mli *.ml >> .depend

dynamics.cmi: dyntypes_compiler_bits.cmi
dynamics.cmo: dyntypes_compiler_bits.cmo
dynamics.cmx: dyntypes_compiler_bits.cmx
dyntypes_compiler_bits.cmi: dyntypes_compiler_bits.cmo ;

include .depend
