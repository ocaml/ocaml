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

# Makefile for the str library

include ../../config/Makefile

<<<<<<< .courant
# Compilation options
CC=$(BYTECC)
CFLAGS=-O -I../../byterun $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)
CAMLC=../../ocamlcomp.sh
CAMLOPT=../../ocamlcompopt.sh
COMPFLAGS=-warn-error A -nojoin -g
COBJS=strstubs.o
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
=======
LIBNAME=str
COBJS=strstubs.$(O)
CLIBNAME=camlstr
CAMLOBJS=str.cmo
>>>>>>> .fusion-droit.r10497

all: libstr.a str.cmi str.cma

allopt: libstr.a str.cmi str.cmxa

<<<<<<< .courant
libstr.a: $(COBJS)
	$(MKLIB) -o str $(COBJS)
=======
str.cmo: str.cmi
str.cmx: str.cmi
>>>>>>> .fusion-droit.r10497

str.cma: str.cmo
	$(MKLIB) -ocamlc '$(CAMLC)' -o str str.cmo

str.cmxa: str.cmx
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o str str.cmx

str.cmx: ../../ocamlopt

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.so *.o

install:
	if test -f dllstr.so; then cp dllstr.so $(STUBLIBDIR)/dllstr.so; fi
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
