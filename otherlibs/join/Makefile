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

CAMLC=../../ocamlcomp.sh $(NOJOIN) -I ../unix
CAMLOPT=../../ocamlcompopt.sh $(NOJOIN) -I ../unix
JCC=../../ocamlcomp.sh -I ../unix
JCOPT=../../ocamlcompopt.sh -I ../unix
COMPFLAGS=-warn-error A

STDJOIN_OBJS=joinCom.cmo joinProc.cmo joinTextProc.cmo \
 joinFifo.cmo joinCount.cmo \
 joinHelper.cmo joinPool.cmo joinMapRed.cmo
STDJOIN_MLI=$(STDJOIN_OBJS:.cmo=.mli)
STDJOIN_CMI=$(STDJOIN_OBJS:.cmo=.cmi)
JOIN_OBJS=site.cmo ns.cmo join.cmo $(STDJOIN_OBJS)
JOIN_X=$(JOIN_OBJS:.cmo=.cmx)

VMTHREADSLIB=../threads
VMTHREADSLIB_OBJS=$(VMTHREADSLIB)/threads.cma

SYSTHREADSLIB=../systhreads
SYSTHREADSLIB_OBJS=$(SYSTHREADSLIB)/threads.cma

THREADS=../systhreads

all: vmjoin.cma join.cma 

allopt: join.cmxa

$(JOIN_OBJS): join.cmi $(VMTHREADSLIB_OBJS) $(SYSTHREADSLIB_OBJS)

$(JOIN_X): join.cmi $(SYSTHREADSLIB)/threads.cmxa

vmjoin.cma: $(JOIN_OBJS) 
	if test -f $(VMTHREADSLIB_OBJS); then \
		$(CAMLC) -a -o vmjoin.cma  $(JOIN_OBJS); \
	fi


join.cma: $(JOIN_OBJS) 
	$(CAMLC) -a -o join.cma $(JOIN_OBJS)

join.cmxa: $(JOIN_X)
	$(CAMLOPT) -a -o join.cmxa $(JOIN_X)

partialclean::
	/bin/rm -f join.{cma,cmxa,a} vmjoin.cma
	rm -f $(JOIN_OBJS) $(JOIN_X) *.cmi *.o

clean: partialclean

install:
	cp join.mli join.cmi $(LIBDIR)/vmthreads
	cp $(STDJOIN_MLI) $(STDJOIN_CMI) $(LIBDIR)/vmthreads
	cp vmjoin.cma $(LIBDIR)/vmthreads/join.cma
	cp join.mli join.cmi $(LIBDIR)/threads
	cp  $(STDJOIN_MLI) $(STDJOIN_CMI) $(LIBDIR)/threads
	cp join.cma $(LIBDIR)/threads/join.cma

installopt:
	cp join.cmxa join.a $(LIBDIR)/threads

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) -I $(THREADS) $<

.ml.cmo:
	$(JCC) -c $(COMPFLAGS) -I $(THREADS) $(OCAMLPP) $<

.ml.cmx:
	$(JCOPT) -c $(COMPFLAGS) -I $(THREADS) $(OCAMLPP) $<

depend:
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml > .depend

include .depend
