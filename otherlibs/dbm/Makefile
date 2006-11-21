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
COMPFLAGS=-warn-error A -nojoin

CFLAGS=$(DBM_INCLUDES) -I../../byterun -O $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)
COBJS=cldbm.o

all: libmldbm.a dbm.cmi dbm.cma

allopt: libmldbm.a dbm.cmi dbm.cmxa

libmldbm.a: $(COBJS)
	$(MKLIB) -oc mldbm $(COBJS) $(DBM_LINK)

dbm.cma: dbm.cmo
	$(MKLIB) -ocamlc '$(CAMLC)' -o dbm -oc mldbm dbm.cmo $(DBM_LINK)

dbm.cmxa: dbm.cmx
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o dbm -oc mldbm dbm.cmx $(DBM_LINK)

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o *.so

install:
	if test -f dllmldbm.so; then cp dllmldbm.so $(STUBLIBDIR)/dllmldbm.so; fi
	cp libmldbm.a $(LIBDIR)/libmldbm.a
	cd $(LIBDIR); $(RANLIB) libmldbm.a
	cp dbm.cma dbm.cmi dbm.mli $(LIBDIR)

installopt:
	cp dbm.cmx dbm.cmxa dbm.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) dbm.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml > .depend

include .depend
