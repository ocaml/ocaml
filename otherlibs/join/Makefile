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


JOIN_OBJS=site.cmo ns.cmo join.cmo

LIB=../../stdlib

LIB_OBJS=pervasives.cmo \
  $(LIB)/array.cmo $(LIB)/list.cmo $(LIB)/char.cmo $(LIB)/string.cmo \
  $(LIB)/sys.cmo $(LIB)/hashtbl.cmo $(LIB)/sort.cmo \
  marshal.cmo $(LIB)/obj.cmo $(LIB)/int32.cmo $(LIB)/int64.cmo \
  $(LIB)/nativeint.cmo \
  $(LIB)/lexing.cmo $(LIB)/parsing.cmo \
  $(LIB)/set.cmo $(LIB)/map.cmo $(LIB)/stack.cmo $(LIB)/queue.cmo \
  $(LIB)/stream.cmo $(LIB)/buffer.cmo \
  $(LIB)/printf.cmo $(LIB)/format.cmo \
  $(LIB)/scanf.cmo $(LIB)/arg.cmo \
  $(LIB)/printexc.cmo $(LIB)/gc.cmo $(LIB)/digest.cmo $(LIB)/random.cmo \
  $(LIB)/camlinternalOO.cmo $(LIB)/oo.cmo $(LIB)/camlinternalMod.cmo \
  $(LIB)/genlex.cmo $(LIB)/callback.cmo $(LIB)/weak.cmo \
  $(LIB)/lazy.cmo $(LIB)/filename.cmo $(LIB)/complex.cmo \
  $(LIB)/arrayLabels.cmo $(LIB)/listLabels.cmo $(LIB)/stringLabels.cmo \
  $(LIB)/stdLabels.cmo $(LIB)/moreLabels.cmo


VMTHREADSLIB=../threads
VMTHREADSLIB_OBJS=$(VMTHREADSLIB)/threads.cma

SYSTHREADSLIB=../systhreads
SYSTHREADSLIB_OBJS=$(SYSTHREADSLIB)/threads.cma

THREADS=../systhreads

all: vmjoin.cma join.cma

allopt: join.cmxa

vmjoin.cma: $(JOIN_OBJS) 
	if test -f $(VMTHREADSLIB_OBJS); then \
		$(CAMLC) -a -o vmjoin.cma $(VMTHREADSLIB_OBJS) $(JOIN_OBJS); \
	fi


join.cma: $(JOIN_OBJS) 
	$(CAMLC) -a -o join.cma $(SYSTHREADSLIB_OBJS) $(JOIN_OBJS)

join.cmxa: $(JOIN_OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o join.cmxa \
		$(SYSTHREADSLIB_OBJS:.cma=.cmxa) $(JOIN_OBJS:.cmo=.cmx)




partialclean:
	rm -f *.cm*

clean: partialclean


install:
#Install the join library (and friends)
	cp join.mli join.cmi $(LIBDIR)/vmthreads
	cp vmjoin.cma $(LIBDIR)/vmthreads/join.cma
	cp join.mli join.cmi $(LIBDIR)/threads
	cp join.cma $(LIBDIR)/threads/join.cma

installopt:
	cp join.cmxa $(LIBDIR)/threads/join.cmxa

.SUFFIXES: .jml .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) -I $(THREADS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) -I $(THREADS) $(OCAMLPP) $<

.jml.cmo:
	$(JCC) -c $(COMPFLAGS) -I $(THREADS) $(OCAMLPP) -impl $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) -I $(THREADS) $<
	$(CAMLOPT) -c $(COMPFLAGS) -I $(THREADS) $(OCAMLPP) $<

depend:
	/bin/rm -f ns.ml
	../../boot/ocamlrun ../../tools/ocamldep -nojoin *.mli *.ml >> .depend
	cp ns.jml ns.ml 
	../../boot/ocamlrun ../../tools/ocamldep ns.ml >> .depend
	/bin/rm ns.ml

include .depend
