#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License.          #
#                                                                       #
#########################################################################

# $Id$

include ../../config/Makefile

CC=$(BYTECC)
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS) -g
CAMLC=../../boot/ocamlrun ../../ocamlc -I ../../stdlib -I ../unix

C_OBJS=scheduler.o

CAML_OBJS=thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo

LIB=../../stdlib

LIB_OBJS=pervasives.cmo \
  $(LIB)/array.cmo $(LIB)/list.cmo $(LIB)/char.cmo $(LIB)/string.cmo \
  $(LIB)/sys.cmo $(LIB)/hashtbl.cmo $(LIB)/sort.cmo \
  marshal.cmo $(LIB)/obj.cmo \
  $(LIB)/lexing.cmo $(LIB)/parsing.cmo \
  $(LIB)/set.cmo $(LIB)/map.cmo $(LIB)/stack.cmo $(LIB)/queue.cmo \
  $(LIB)/stream.cmo $(LIB)/buffer.cmo \
  $(LIB)/printf.cmo $(LIB)/format.cmo $(LIB)/arg.cmo \
  $(LIB)/printexc.cmo $(LIB)/gc.cmo $(LIB)/digest.cmo $(LIB)/random.cmo \
  $(LIB)/oo.cmo $(LIB)/genlex.cmo $(LIB)/callback.cmo $(LIB)/weak.cmo \
  $(LIB)/lazy.cmo $(LIB)/filename.cmo $(LIB)/int32.cmo $(LIB)/int64.cmo \
  $(LIB)/nativeint.cmo

UNIXLIB=../unix

UNIXLIB_OBJS=unix.cmo

all: libthreads.a threads.cma stdlib.cma unix.cma

allopt:

libthreads.a: $(C_OBJS)
	rm -f libthreads.a
	ar rc libthreads.a $(C_OBJS)
	$(RANLIB) libthreads.a

threads.cma: $(CAML_OBJS)
	$(CAMLC) -a -o threads.cma -custom $(CAML_OBJS) -cclib -lthreads

stdlib.cma: $(LIB_OBJS)
	$(CAMLC) -a -o stdlib.cma $(LIB_OBJS)

unix.cma: $(UNIXLIB_OBJS)
	$(CAMLC) -a -linkall -custom -o unix.cma $(UNIXLIB_OBJS) -cclib -lunix

pervasives.cmo: pervasives.mli pervasives.cmi pervasives.ml
	$(CAMLC) -nopervasives -c pervasives.ml

pervasives.mli: $(LIB)/pervasives.mli
	ln -s $(LIB)/pervasives.mli pervasives.mli

pervasives.cmi: $(LIB)/pervasives.cmi
	ln -s $(LIB)/pervasives.cmi pervasives.cmi

marshal.cmo: marshal.mli marshal.cmi marshal.ml
	$(CAMLC) -c marshal.ml

marshal.mli: $(LIB)/marshal.mli
	ln -s $(LIB)/marshal.mli marshal.mli

marshal.cmi: $(LIB)/marshal.cmi
	ln -s $(LIB)/marshal.cmi marshal.cmi

unix.cmo: unix.mli unix.cmi unix.ml
	$(CAMLC) -c unix.ml

unix.mli: $(UNIXLIB)/unix.mli
	ln -s $(UNIXLIB)/unix.mli unix.mli

unix.cmi: $(UNIXLIB)/unix.cmi
	ln -s $(UNIXLIB)/unix.cmi unix.cmi

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f libthreads.a *.o
	rm -f pervasives.mli marshal.mli unix.mli

install:
	cp libthreads.a $(LIBDIR)/libthreads.a
	cd $(LIBDIR); $(RANLIB) libthreads.a
	if test -d $(LIBDIR)/threads; then : ; else mkdir $(LIBDIR)/threads; fi
	cp thread.cmi mutex.cmi condition.cmi event.cmi threadUnix.cmi threads.cma stdlib.cma unix.cma $(LIBDIR)/threads
	cp thread.mli mutex.mli condition.mli event.mli threadUnix.mli $(LIBDIR)

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
