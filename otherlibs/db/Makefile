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
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS)
COMPFLAGS=-warn-error A
COBJS=dbstubs.o

all: libmldb.a db.cmi db.cma

allopt: libmldb.a db.cmi db.cmxa

libmldb.a: $(COBJS)
	rm -rf libmldb.a
	ar rc libmldb.a $(COBJS)
	$(RANLIB) libmldb.a

db.cma: db.cmo
	$(CAMLC) -a -o db.cma -custom db.cmo -cclib -lmldb

db.cmxa: db.cmx
	$(CAMLOPT) -a -o db.cmxa db.cmx -cclib -lmldb

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o

install:
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
