#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../../LICENSE.   #
#                                                                       #
#########################################################################

include ../../config/Makefile

CC=$(BYTECC)
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS) -g
CAMLC=../../ocamlcomp.sh -I ../unix
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
COMPFLAGS=-w +33..39 -warn-error A

C_OBJS=scheduler.o

CAML_OBJS=thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo

LIB=../../stdlib

LIB_OBJS=pervasives.cmo \
  $(LIB)/array.cmo $(LIB)/list.cmo $(LIB)/char.cmo $(LIB)/string.cmo        \
  $(LIB)/sys.cmo $(LIB)/sort.cmo marshal.cmo $(LIB)/obj.cmo                 \
  $(LIB)/int32.cmo $(LIB)/int64.cmo $(LIB)/nativeint.cmo                    \
  $(LIB)/lexing.cmo $(LIB)/parsing.cmo $(LIB)/set.cmo $(LIB)/map.cmo        \
  $(LIB)/stack.cmo $(LIB)/queue.cmo $(LIB)/camlinternalLazy.cmo             \
  $(LIB)/lazy.cmo $(LIB)/stream.cmo $(LIB)/buffer.cmo                       \
  $(LIB)/printf.cmo $(LIB)/arg.cmo $(LIB)/printexc.cmo $(LIB)/gc.cmo        \
  $(LIB)/digest.cmo $(LIB)/random.cmo $(LIB)/hashtbl.cmo                    \
  $(LIB)/format.cmo $(LIB)/scanf.cmo $(LIB)/callback.cmo \
  $(LIB)/camlinternalOO.cmo              \
  $(LIB)/oo.cmo $(LIB)/camlinternalMod.cmo $(LIB)/genlex.cmo                \
  $(LIB)/weak.cmo $(LIB)/filename.cmo                   \
  $(LIB)/complex.cmo $(LIB)/arrayLabels.cmo $(LIB)/listLabels.cmo           \
  $(LIB)/stringLabels.cmo $(LIB)/stdLabels.cmo $(LIB)/moreLabels.cmo

UNIXLIB=../unix

UNIXLIB_OBJS=unix.cmo $(UNIXLIB)/unixLabels.cmo

all: libvmthreads.a threads.cma stdlib.cma unix.cma

allopt:

libvmthreads.a: $(C_OBJS)
	$(MKLIB) -o threads -oc vmthreads $(C_OBJS)

threads.cma: $(CAML_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o threads -oc vmthreads $(CAML_OBJS)

stdlib.cma: $(LIB_OBJS)
	$(CAMLC) -a -o stdlib.cma $(LIB_OBJS)

unix.cma: $(UNIXLIB_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o unix -linkall $(UNIXLIB_OBJS)

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
	ln -s -f $(UNIXLIB)/unix.mli unix.mli

unix.cmi: $(UNIXLIB)/unix.cmi
	ln -s -f $(UNIXLIB)/unix.cmi unix.cmi

unix.cmo: unix.mli unix.cmi $(UNIXLIB)/unixLabels.cmo
	$(CAMLC) ${COMPFLAGS} -c unix.ml

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f libvmthreads.a dllvmthreads.so *.o
	rm -f pervasives.mli marshal.mli unix.mli

install:
	if test -f dllvmthreads.so; then cp dllvmthreads.so $(STUBLIBDIR)/.; fi
	mkdir -p $(LIBDIR)/vmthreads
	cp libvmthreads.a $(LIBDIR)/vmthreads/libvmthreads.a
	cd $(LIBDIR)/vmthreads; $(RANLIB) libvmthreads.a
	cp thread.cmi mutex.cmi condition.cmi event.cmi threadUnix.cmi \
	   threads.cma stdlib.cma unix.cma $(LIBDIR)/vmthreads
	cp thread.mli mutex.mli condition.mli event.mli threadUnix.mli \
	   $(LIBDIR)/vmthreads

installopt:

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
