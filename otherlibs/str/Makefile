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

# Makefile for the str library

include ../../config/Makefile

# Compilation options
CC=$(BYTECC)
CFLAGS=-O -I$(REGEXLIB) -I../../byterun $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)
CAMLC=../../boot/ocamlrun ../../ocamlc -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib
REGEXLIB=regex-0.12
COBJS=strstubs.o $(REGEXLIB)/regex.o
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib

all: libstr.a str.cmi str.cma

allopt: libstr.a str.cmi str.cmxa

libstr.a: $(COBJS)
	$(MKLIB) -o str $(COBJS)

str.cma: str.cmo
	$(MKLIB) -ocamlc '$(CAMLC)' -o str str.cmo

str.cmxa: str.cmx
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o str str.cmx

$(REGEXLIB)/regex.o: $(REGEXLIB)/regex.c $(REGEXLIB)/regex.h
	cd $(REGEXLIB); CC="$(CC) $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)" sh configure; $(MAKE)

str.cmx: ../../ocamlopt

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.so *.o
	cd $(REGEXLIB); if test -f Makefile; then $(MAKE) distclean; else exit 0; fi

install:
	if test -f dllstr.so; then cp dllstr.so $(LIBDIR)/dllstr.so; fi
	cp libstr.a $(LIBDIR)/libstr.a
	cd $(LIBDIR); $(RANLIB) libstr.a
	cp str.cma str.cmi str.mli $(LIBDIR)

installopt:
	cp str.cmx str.cmxa str.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) str.a

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
