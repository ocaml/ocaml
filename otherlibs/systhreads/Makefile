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

CAMLC=../../ocamlcomp.sh -I ../unix
CAMLOPT=../../ocamlcompopt.sh -I ../unix
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
COMPFLAGS=-warn-error A -g

BYTECODE_C_OBJS=posix_b.o
NATIVECODE_C_OBJS=posix_n.o

THREAD_OBJS= thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo

GENFILES=thread.ml

all: libthreads.a threads.cma

allopt: libthreadsnat.a threads.cmxa

libthreads.a: $(BYTECODE_C_OBJS)
	$(MKLIB) -o threads $(BYTECODE_C_OBJS)

posix_b.o: posix.c
	$(BYTECC) -O -I../../byterun $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS) \
	   -c posix.c
	mv posix.o posix_b.o

# Dynamic linking with -lpthread is risky on many platforms, so
# do not create a shared object for libthreadsnat.
libthreadsnat.a: $(NATIVECODE_C_OBJS)
	$(AR) rc libthreadsnat.a $(NATIVECODE_C_OBJS)

posix_n.o: posix.c
	$(NATIVECC) -O -I../../asmrun -I../../byterun $(NATIVECCCOMPOPTS) $(SHAREDCCCOMPOPTS) -DNATIVE_CODE -DTARGET_$(ARCH) -DSYS_$(SYSTEM) -c posix.c
	mv posix.o posix_n.o

threads.cma: $(THREAD_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o threads $(THREAD_OBJS) \
          -cclib -lunix $(PTHREAD_LINK)

# See remark above: force static linking of libthreadsnat.a
threads.cmxa: $(THREAD_OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o threads.cmxa $(THREAD_OBJS:.cmo=.cmx) \
          -cclib -lthreadsnat $(PTHREAD_LINK)

# Note: I removed "-cclib -lunix" from the line above. 
# Indeed, if we link threads.cmxa, then we must also link unix.cmxa, 
# which itself will pass -lunix to the C linker.  It seems more
# modular to me this way. -- Alain


$(THREAD_OBJS:.cmo=.cmx): ../../ocamlopt

thread.ml: thread_posix.ml
	ln -s thread_posix.ml thread.ml

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.o *.a *.so
	rm -f $(GENFILES)

install:
	if test -f dllthreads.so; then cp dllthreads.so $(STUBLIBDIR)/dllthreads.so; fi
	cp libthreads.a $(LIBDIR)/libthreads.a
	cd $(LIBDIR); $(RANLIB) libthreads.a
	if test -d $(LIBDIR)/threads; then :; else mkdir $(LIBDIR)/threads; fi
	cp $(THREAD_OBJS:.cmo=.cmi) threads.cma $(LIBDIR)/threads
	rm -f $(LIBDIR)/threads/stdlib.cma
	cp thread.mli mutex.mli condition.mli event.mli threadUnix.mli $(LIBDIR)

installopt:
	cp libthreadsnat.a $(LIBDIR)/libthreadsnat.a
	cd $(LIBDIR); $(RANLIB) libthreadsnat.a
	cp $(THREAD_OBJS:.cmo=.cmx) threads.cmxa threads.a $(LIBDIR)/threads
	cd $(LIBDIR)/threads; $(RANLIB) threads.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend: $(GENFILES)
	-gcc -MM -I../../byterun *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
