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

include ../../config/Makefile

CC=$(BYTECC)
CFLAGS=-I../../byterun -g -O $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)
CAMLC=../../ocamlcomp.sh -I ../unix
CAMLOPT=../../ocamlcompopt.sh -I ../unix
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
COMPFLAGS=-warn-error A

C_OBJS=bigarray_stubs.o mmap_unix.o

CAML_OBJS=bigarray.cmo

all: libbigarray.a bigarray.cma

allopt: libbigarray.a bigarray.cmxa

libbigarray.a: $(C_OBJS)
	$(MKLIB) -o bigarray $(C_OBJS)

bigarray.cma: $(CAML_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -linkall -o bigarray $(CAML_OBJS)

bigarray.cmxa: $(CAML_OBJS:.cmo=.cmx)
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -linkall -o bigarray \
          $(CAML_OBJS:.cmo=.cmx)

install:
	if test -f dllbigarray.so; then cp dllbigarray.so $(STUBLIBDIR)/dllbigarray.so; fi
	cp bigarray.cmi bigarray.mli libbigarray.a bigarray.cma $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) libbigarray.a
	cp bigarray.h $(LIBDIR)/caml/bigarray.h

installopt:
	cp bigarray.a $(CAML_OBJS:.cmo=.cmx) bigarray.cmxa $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) bigarray.a

partialclean:
	rm -f *.cm* 

clean: partialclean
	rm -f libbigarray.* *.o bigarray.a *.so

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	gcc -MM -I../../byterun -I../unix *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
