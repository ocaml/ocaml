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
JOCPARSER=../../boot/ocamlrun ../../jocparsing/jocp
CAMLC=../../boot/ocamlrun ../../ocamlc -pp "$(JOCPARSER)" -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -pp "$(JOCPARSER)" -I ../../stdlib
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib

LOCAL_MODULES = dynamics
ROOT = ../..
UTILS_MODULES = config misc tbl
PARSING_MODULES = longident
TYPING_MODULES = path ident primitive types btype subst ctype datarepr env predef printtyp
IMPORTED_MODULES = $(UTILS_MODULES) $(PARSING_MODULES) $(TYPING_MODULES)
IMPORTED_INTERFACES = $(IMPORTED_MODULES) asttypes outcometree
CMIFILES = $(IMPORTED_MODULES:=.cmi) $(LOCAL_MODULES:=.cmi)
CAMLOBJS = clflags.cmo $(IMPORTED_MODULES:=.cmo) $(LOCAL_MODULES:=.cmo)

default: all

import:
	set -e; \
	for x in $(UTILS_MODULES); do \
	  cp -f $(ROOT)/utils/$$x.mli $(ROOT)/utils/$$x.ml .; \
	done; \
	for x in $(PARSING_MODULES); do \
	  cp -f $(ROOT)/parsing/$$x.mli $(ROOT)/parsing/$$x.ml .; \
	done; \
	for x in $(TYPING_MODULES); do \
	  cp -f $(ROOT)/typing/$$x.mli $(ROOT)/typing/$$x.ml .; \
	done; \
	cp -f $(ROOT)/utils/clflags.ml .; \
	cp -f $(ROOT)/parsing/asttypes.mli $(ROOT)/typing/outcometree.mli .
	touch import
do-import: do_import
do_import:
	rm -f import
	$(MAKE) import
	: >.depend
	$(MAKE) depend

all: dynamics.cma $(CMIFILES)

allopt: dynamics.cmxa $(CMIFILES)

dynamics.cma: $(CAMLOBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o dynamics $(CAMLOBJS)

dynamics.cmxa: $(CAMLOBJS:.cmo=.cmx)
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o dynamics $(CAMLOBJS:.cmo=.cmx)

$(IMPORTED_INTERFACES:=.mli): import
$(IMPORTED_MODULES:=.ml): import
$(CAMLOBJS:.cmo=.cmx): ../../ocamlopt

install:
	cp dynamics.cma $(CMIFILES) $(CMIFILES:.cmi=.mli) $(LIBDIR)

installopt:
	cp $(CAMLOBJS:.cmo=.cmx) dynamics.cmxa $(LIBDIR)

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f import $(IMPORTED_MODULES:=.ml) $(IMPORTED_INTERFACES:=.mli)
	rm -f *.a *.o *.so

.SUFFIXES: .ml .mli .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	: >.depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
clflags.cmi: clflags.cmo
