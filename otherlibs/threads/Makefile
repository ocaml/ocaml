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
CAMLC=../../ocamlcomp.sh -I ../unix
JCC=$(CAMLC) -join
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
COMPFLAGS=-warn-error A

C_OBJS=scheduler.o

CAML_OBJS=thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo

JOIN_C_OBJS=join.o

JOIN_OBJS=join_debug.cmo join_misc.cmo join_link.cmo  join_hash.cmo\
 join_queue.cmo join_scheduler.cmo join_port.cmo\
 join_message.cmo join_space.cmo join.cmo ns.cmo

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
	$(MKLIB) -ocamlc '$(CAMLC)' -o threads -oc vmthreads $(CAML_OBJS)

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

unix.cmo: unix.mli unix.cmi unix.ml
	$(CAMLC) ${COMPFLAGS} -c unix.ml

unix.mli: $(UNIXLIB)/unix.mli
	ln -s $(UNIXLIB)/unix.mli unix.mli

unix.cmi: $(UNIXLIB)/unix.cmi
	ln -s $(UNIXLIB)/unix.cmi unix.cmi

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f libvmthreads.a dllvmthreads.so *.o
	rm -f libvmjoin.a dllvmjoin.so *.o
	rm -f pervasives.mli marshal.mli unix.mli

install:
	if test -f dllvmthreads.so; then cp dllvmthreads.so $(STUBLIBDIR)/.; fi
	if test -f dllvmjoin.so; then cp dllvmjoin.so $(STUBLIBDIR)/.; fi
	mkdir -p $(LIBDIR)/vmthreads
	cp libvmthreads.a $(LIBDIR)/vmthreads/libvmthreads.a
	cd $(LIBDIR)/vmthreads; $(RANLIB) libvmthreads.a
	cp thread.cmi mutex.cmi condition.cmi event.cmi threadUnix.cmi threads.cma stdlib.cma unix.cma $(LIBDIR)/vmthreads
	cp libvmjoin.a $(LIBDIR)/vmthreads/libvmjoin.a
	cd $(LIBDIR)/vmthreads; $(RANLIB) libvmjoin.a
	cp thread.cmi mutex.cmi condition.cmi event.cmi threadUnix.cmi join_space.cmi join.cmi ns.cmi threads.cma stdlib.cma unix.cma join.cma $(LIBDIR)/vmthreads
	cp thread.mli mutex.mli condition.mli event.mli threadUnix.mli $(LIBDIR)/vmthreads

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
	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend
	cp ns.jml ns.ml 
	../../boot/ocamlrun ../../tools/ocamldep -join ns.ml >> .depend
	/bin/rm ns.ml

include .depend
