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

CAMLC=../../boot/ocamlrun ../../ocamlc -I ../../stdlib -I ../unix
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib -I ../unix
MKLIB=../../tools/ocamlmklib

BYTECODE_C_OBJS=posix_b.o
NATIVECODE_C_OBJS=posix_n.o

THREAD_OBJS= thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo

GENFILES=thread.ml

all: libthreads.a threads.cma

allopt: libthreadsnat.a threads.cmxa

libthreads.a: $(BYTECODE_C_OBJS)
	$(MKLIB) -o threads $(BYTECODE_C_OBJS) $(PTHREAD_LINK)

posix_b.o: posix.c
	$(BYTECC) -O -I../../byterun $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS) \
	   -c posix.c
	mv posix.o posix_b.o

libthreadsnat.a: $(NATIVECODE_C_OBJS)
	$(MKLIB) -o threadsnat $(NATIVECODE_C_OBJS) $(PTHREAD_LINK)

posix_n.o: posix.c
	$(NATIVECC) -O -I../../asmrun -I../../byterun $(NATIVECCCOMPOPTS) -DNATIVE_CODE -DTARGET_$(ARCH) -DSYS_$(SYSTEM) -c posix.c
	mv posix.o posix_n.o

threads.cma: $(THREAD_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o threads $(THREAD_OBJS) \
          -cclib -lunix $(PTHREAD_LINK)

threads.cmxa: $(THREAD_OBJS:.cmo=.cmx)
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o threads -oc threadsnat \
          $(THREAD_OBJS:.cmo=.cmx) -cclib -lunix $(PTHREAD_LINK)

$(THREAD_OBJS:.cmo=.cmx): ../../ocamlopt

thread.ml: thread_posix.ml
	ln -s thread_posix.ml thread.ml

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f libthreads*.a *.o threads.a
	rm -f $(GENFILES)

install:
	test -f libthreads.so && cp libthreads.so $(LIBDIR)/libthreads.so
	cp libthreads.a $(LIBDIR)/libthreads.a
	cd $(LIBDIR); $(RANLIB) libthreads.a
	if test -d $(LIBDIR)/threads; then :; else mkdir $(LIBDIR)/threads; fi
	cp $(THREAD_OBJS:.cmo=.cmi) threads.cma $(LIBDIR)/threads
	rm -f $(LIBDIR)/threads/stdlib.cma
	cp thread.mli mutex.mli condition.mli event.mli threadUnix.mli $(LIBDIR)

installopt:
	test -f libthreadsnat.so && cp libthreadsnat.so $(LIBDIR)/libthreadsnat.so
	cp libthreadsnat.a $(LIBDIR)/libthreadsnat.a
	cd $(LIBDIR); $(RANLIB) libthreadsnat.a
	cp $(THREAD_OBJS:.cmo=.cmx) threads.cmxa threads.a $(LIBDIR)/threads
	cd $(LIBDIR)/threads; $(RANLIB) threads.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c -g $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend: $(GENFILES)
	gcc -MM -I../../byterun *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
