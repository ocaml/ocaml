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

CAMLOBJS=dynamics.cmo

CMIFILES=dynamics.cmi

all: dynamics.cma $(CMIFILES)

allopt: dynamics.cmxa $(CMIFILES)

dynamics.cma: $(CAMLOBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o dynamics $(CAMLOBJS)

dynamics.cmxa: $(CAMLOBJS:.cmo=.cmx)
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o dynamics $(CAMLOBJS:.cmo=.cmx)

$(CAMLOBJS:.cmo=.cmx): ../../ocamlopt

install:
	cp dynamics.cma $(CMIFILES) $(CMIFILES:.cmi=.mli) $(LIBDIR)

installopt:
	cp $(CAMLOBJS:.cmo=.cmx) dynamics.cmxa $(LIBDIR)

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o *.so

.SUFFIXES: .ml .mli .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
