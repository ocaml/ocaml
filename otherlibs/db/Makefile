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

# Makefile for the ndbm library

include ../../config/Makefile

# Compilation optiosn
CC=$(BYTECC) -g
CAMLC=../../ocamlcomp.sh
CAMLOPT=../../ocamlcompopt.sh
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
COMPFLAGS=-warn-error A

CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)
COBJS=dbstubs.o

all: libmldb.a db.cmi db.cma

allopt: libmldb.a db.cmi db.cmxa

libmldb.a: $(COBJS)
	$(MKLIB) -oc mldb $(COBJS) $(DB_LINK)

db.cma: db.cmo
	$(MKLIB) -ocamlc '$(CAMLC)' -o db -oc mldb db.cmo $(DB_LINK)

db.cmxa: db.cmx
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o db -oc mldb db.cmx $(DB_LINK)

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o *.so

install:
	if test -f dllmldb.so; then cp dllmldb.so $(STUBLIBDIR)/dllmldb.so; fi
	cp libmldb.a $(LIBDIR)/libmldb.a
	cd $(LIBDIR); $(RANLIB) libmldb.a
	cp db.cma db.cmi db.mli $(LIBDIR)

installopt:
	cp db.cmx db.cmxa db.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) db.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	../../tools/ocamldep *.mli *.ml > .depend

include .depend
