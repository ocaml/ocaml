#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License.          #
#                                                                       #
#########################################################################

# $Id$

include ../../config/Makefile

CC=$(BYTECC)
CFLAGS=-I../../byterun -g -O $(BYTECCCOMPOPTS)
CAMLC=../../boot/ocamlrun ../../ocamlc -I ../../stdlib -I ../unix
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib -I ../unix

C_OBJS=bigarray_stubs.o

CAML_OBJS=bigarray.cmo

all: libbigarray.a bigarray.cma

allopt: libbigarray.a bigarray.cmxa

libbigarray.a: $(C_OBJS)
	rm -f libbigarray.a
	ar rc libbigarray.a $(C_OBJS)
	$(RANLIB) libbigarray.a

bigarray.cma: $(CAML_OBJS)
	$(CAMLC) -a -o bigarray.cma $(CAML_OBJS)

bigarray.cmxa: $(CAML_OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o bigarray.cmxa $(CAML_OBJS:.cmo=.cmx)

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f libbigarray.a *.o

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
