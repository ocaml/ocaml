#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Luc Maranget, projet Moscova, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2001 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License.          #
#                                                                       #
#########################################################################
# $Id$

include ../../config/Makefile

INCLUDES=-I ../../stdlib -I ../threads -I ../systhreads
RUNTIME=../../boot/ocamlrun
COMPILER=../../ocamlc
OPTCOMPILER=../../ocamlopt
CAMLC=$(RUNTIME) $(COMPILER) $(INCLUDES)
CAMLOPT=$(RUNTIME) $(OPTCOMPILER) $(INCLUDES)
MKLIB=$(RUNTIME) ../../tools/ocamlmklib
CC=$(BYTECC) -g
CFLAGS=  -I../../byterun -O $(BYTECCCOMPOPTS)

MLOBJS=weakarray.cmo  weakset.cmo  whashtbl.cmo jprims.cmo
OBJS=weakarray_c.o  weakset_c.o


all: join.cma libjoin.a

allopt: join.cmxa libjoin.a

libjoin.a: $(OBJS)
	$(MKLIB) -o join $(OBJS)

join.cma: $(MLOBJS)
	$(MKLIB) -o join -ocamlc '$(CAMLC)' -linkall $(MLOBJS)

join.cmxa: $(MLOBJS:.cmo=.cmx)
	$(MKLIB) -o join -ocamlopt '$(CAMLOPT)' -linkall $(MLOBJS:.cmo=.cmx)

partialclean:
	/bin/rm -f *~ *.cm? *.cmxa

clean: partialclean
	/bin/rm -f *.o *.a *.so

installopt:

install:
	cp jprims.cmi $(LIBDIR)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
#	gcc -MM $(CFLAGS) *.c > .depend
	$(RUNTIME) ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
