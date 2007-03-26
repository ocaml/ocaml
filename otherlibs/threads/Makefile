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
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS) -g
COMPILER=../../ocamlcomp.sh -I ../unix $(NOJOIN)
CAMLC=../../ocamlcomp.sh -I ../unix $(NOJOIN)
JCC=../../ocamlcomp.sh -I ../unix 
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
COMPFLAGS=-warn-error A

C_OBJS=scheduler.o

CAML_OBJS=thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo

JOIN_C_OBJS=join.o

JOIN_OBJS=join_debug.cmo join_misc.cmo join_link.cmo  join_hash.cmo\
 join_set.cmo join_queue.cmo join_extern.cmo join_scheduler.cmo join_port.cmo\
 join_message.cmo join_space.cmo join_prim.cmo join.cmo ns.cmo join_test.cmo

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

UNIXLIB=../unix

UNIXLIB_OBJS=unix.cmo $(UNIXLIB)/unixLabels.cmo

all: libvmthreads.a threads.cma stdlib.cma unix.cma libvmjoin.a join.cma

allopt:

libvmthreads.a: $(C_OBJS)
	$(MKLIB) -o threads -oc vmthreads $(C_OBJS)

threads.cma: $(CAML_OBJS)
	$(MKLIB) -ocamlc '$(COMPILER)' -o threads -oc vmthreads $(CAML_OBJS)

stdlib.cma: $(LIB_OBJS)
	$(CAMLC) -a -o stdlib.cma $(LIB_OBJS)

unix.cma: $(UNIXLIB_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o unix -linkall $(UNIXLIB_OBJS)

libvmjoin.a: $(JOIN_C_OBJS)
	$(MKLIB) -o join -oc vmjoin $(JOIN_C_OBJS)

join.cma: $(JOIN_OBJS) unix.cma threads.cma
	$(MKLIB) -ocamlc '$(CAMLC)' -o join -oc vmjoin unix.cma threads.cma $(JOIN_OBJS)

pervasives.cmo: pervasives.mli pervasives.cmi pervasives.ml
	$(CAMLC) ${COMPFLAGS} -nopervasives -c pervasives.ml

pervasives.mli: $(LIB)/pervasives.mli
	ln -s $(LIB)/pervasives.mli pervasives.mli

pervasives.cmi: $(LIB)/pervasives.cmi
	ln -s $(LIB)/pervasives.cmi pervasives.cmi

marshal.cmo: marshal.mli marshal.cmi marshal.ml
	$(CAMLC) ${COMPFLAGS} -c marshal.ml

marshal.mli: $(LIB)/marshal.mli
	ln -s $(LIB)/marshal.mli marshal.mli

marshal.cmi: $(LIB)/marshal.cmi
	ln -s $(LIB)/marshal.cmi marshal.cmi

unix.mli: $(UNIXLIB)/unix.mli
	ln -sf $(UNIXLIB)/unix.mli unix.mli

unix.cmi: $(UNIXLIB)/unix.cmi
	ln -sf $(UNIXLIB)/unix.cmi unix.cmi

unix.cmo: unix.mli unix.cmi $(UNIXLIB)/unixLabels.cmo 
	$(CAMLC) ${COMPFLAGS} -c unix.ml

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f libvmthreads.a dllvmthreads.so *.o
	rm -f libvmjoin.a dllvmjoin.so *.o
	rm -f pervasives.mli marshal.mli unix.mli

install:
	if test -f dllvmthreads.so; then cp dllvmthreads.so $(STUBLIBDIR)/.; fi
	mkdir -p $(LIBDIR)/vmthreads
	cp libvmthreads.a $(LIBDIR)/vmthreads/libvmthreads.a
	cd $(LIBDIR)/vmthreads; $(RANLIB) libvmthreads.a
	cp thread.cmi mutex.cmi condition.cmi event.cmi threadUnix.cmi threads.cma stdlib.cma unix.cma $(LIBDIR)/vmthreads
	cp thread.mli mutex.mli condition.mli event.mli threadUnix.mli $(LIBDIR)/vmthreads
#Install the join library (and friends)
	if test -f dllvmjoin.so; then cp dllvmjoin.so $(STUBLIBDIR)/.; fi
	cp libvmjoin.a $(LIBDIR)/vmthreads/libvmjoin.a
	cd $(LIBDIR)/vmthreads; $(RANLIB) libvmjoin.a
	cp join_prim.cmi join.cmi ns.cmi join_test.cmi join.cma $(LIBDIR)/vmthreads
	cp join_prim.mli join.mli ns.mli join_test.mli $(LIBDIR)/vmthreads

installopt:

.SUFFIXES: .jml .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $(OCAMLPP) $<

.jml.cmo:
	$(JCC) -c $(COMPFLAGS) $(OCAMLPP) -impl $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<
	$(CAMLOPT) -c $(COMPFLAGS) $(OCAMLPP) $<

depend:
	gcc -MM $(CFLAGS) *.c > .depen
	/bin/rm -f ns.ml
	../../boot/ocamlrun ../../tools/ocamldep -nojoin *.mli *.ml >> .depend
	cp ns.jml ns.ml 
	../../boot/ocamlrun ../../tools/ocamldep ns.ml >> .depend
	/bin/rm ns.ml

include .depend
