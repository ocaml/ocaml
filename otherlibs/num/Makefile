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

# Makefile for the "num" (exact rational arithmetic) library

include ../../config/Makefile

# Compilation options
CC=$(BYTECC)
CFLAGS=-O -I./bignum/h -I../../byterun $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)
CAMLC=../../ocamlcomp.sh -w s
CAMLOPT=../../ocamlcompopt.sh -w s
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
COMPFLAGS=-warn-error A

CAMLOBJS=int_misc.cmo string_misc.cmo nat.cmo big_int.cmo arith_flags.cmo \
  ratio.cmo num.cmo arith_status.cmo

CMIFILES=big_int.cmi nat.cmi num.cmi ratio.cmi arith_status.cmi

COBJS=nat_stubs.o
BIGNUM_OBJS=bignum/o/KerN.o bignum/o/bnInit.o bignum/o/bnMult.o \
  bignum/o/bnDivide.o bignum/o/bnCmp.o bignum/o/bzf.o bignum/o/bz.o

all: libnums.a nums.cma $(CMIFILES)

allopt: libnums.a nums.cmxa $(CMIFILES)

nums.cma: $(CAMLOBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o nums $(CAMLOBJS)

nums.cmxa: $(CAMLOBJS:.cmo=.cmx)
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o nums $(CAMLOBJS:.cmo=.cmx)

libnums.a: $(BIGNUM_OBJS) $(COBJS)
	$(MKLIB) -o nums $(BIGNUM_OBJS) $(COBJS)

$(BIGNUM_OBJS):
	cd bignum; $(MAKE) $(BIGNUM_ARCH) CC="$(CC) $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)"

$(CAMLOBJS:.cmo=.cmx): ../../ocamlopt

install:
	if test -f dllnums.so; then cp dllnums.so $(STUBLIBDIR)/dllnums.so; fi
	cp libnums.a $(LIBDIR)/libnums.a
	cd $(LIBDIR); $(RANLIB) libnums.a
	cp nums.cma $(CMIFILES) $(CMIFILES:.cmi=.mli) $(LIBDIR)

installopt:
	cp $(CAMLOBJS:.cmo=.cmx) nums.cmxa nums.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) nums.a

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o *.so
	cd bignum; $(MAKE) scratch
	cd test; $(MAKE) clean

.SUFFIXES: .ml .mli .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

nat_stubs.o: nat.h

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
