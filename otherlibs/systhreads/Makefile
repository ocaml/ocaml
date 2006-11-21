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
JCC=../../ocamlcomp.sh -I ../unix
JCOPT=../../ocamlcompopt.sh -I ../unix
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
COMPFLAGS=-warn-error A
#OCAMLPP=-pp '../../ocamldoc/remove_DEBUG'

BYTECODE_C_OBJS=posix_b.o
NATIVECODE_C_OBJS=posix_n.o
THREAD_OBJS= thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo

BYTECODE_JOIN_C_OBJS=join_b.o
NATIVECODE_JOIN_C_OBJS=join_n.o

JOIN_OBJS=join_debug.cmo join_misc.cmo join_link.cmo join_hash.cmo\
 join_set.cmo join_queue.cmo join_extern.cmo join_scheduler.cmo join_port.cmo\
 join_message.cmo join_space.cmo join.cmo ns.cmo join_test.cmo

GENFILES=thread.ml join.c join_types.mli\
 join_debug.mli join_debug.ml\
 join_misc.mli join_misc.ml\
 join_link.mli join_link.ml\
 join_port.mli join_port.ml\
 join_hash.mli join_hash.ml\
 join_set.mli join_set.ml\
 join_queue.mli join_queue.ml\
 join_scheduler.mli join_scheduler.ml\
 join_message.mli join_message.ml\
 join_space.mli join_space.ml\
 join.ml join.mli\
 ns.mli ns.jml\
 join_test.mli join_test.ml

all: libthreads.a threads.cma libjoin.a join.cma

allopt: libthreadsnat.a threads.cmxa libjoinnat.a join.cmxa

libthreads.a: $(BYTECODE_C_OBJS)
	$(MKLIB) -o threads $(BYTECODE_C_OBJS)

libjoin.a: $(BYTECODE_JOIN_C_OBJS)
	$(MKLIB) -o join $(BYTECODE_JOIN_C_OBJS)

posix_b.o: posix.c
	$(BYTECC) -O -I../../byterun $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS) \
	   -c posix.c
	mv posix.o posix_b.o

join_b.o: join.c
	$(BYTECC) -DCAML_NAME_SPACE -O -I../../byterun $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS) \
	   -c join.c
	mv join.o join_b.o

# Dynamic linking with -lpthread is risky on many platforms, so
# do not create a shared object for libthreadsnat.
libthreadsnat.a: $(NATIVECODE_C_OBJS)
	$(AR) rc libthreadsnat.a $(NATIVECODE_C_OBJS)

libjoinnat.a: $(NATIVECODE_JOIN_C_OBJS)
	$(AR) rc libjoinnat.a $(NATIVECODE_JOIN_C_OBJS)

posix_n.o: posix.c
	$(NATIVECC) -O -I../../asmrun -I../../byterun $(NATIVECCCOMPOPTS) $(SHAREDCCCOMPOPTS) -DNATIVE_CODE -DTARGET_$(ARCH) -DSYS_$(SYSTEM) -c posix.c
	mv posix.o posix_n.o

join_n.o: join.c
	$(NATIVECC) -DCAML_NAME_SPACE -O -I../../asmrun -I../../byterun $(NATIVECCCOMPOPTS) $(SHAREDCCCOMPOPTS) -DNATIVE_CODE -DTARGET_$(ARCH) -DSYS_$(SYSTEM) -c join.c
	mv join.o join_n.o

threads.cma: $(THREAD_OBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o threads $(THREAD_OBJS) \
          -cclib -lunix $(PTHREAD_LINK)

join.cma: $(JOIN_OBJS) threads.cma ../unix/unix.cma
	$(MKLIB) -ocamlc '$(CAMLC)' -o join ../unix/unix.cma threads.cma $(JOIN_OBJS) \
          -cclib -lunix $(PTHREAD_LINK)

# See remark above: force static linking of libthreadsnat.a
threads.cmxa: $(THREAD_OBJS:.cmo=.cmx)
	$(CAMLOPT) -nojoin -a -o threads.cmxa $(THREAD_OBJS:.cmo=.cmx) \
          -cclib -lthreadsnat -cclib -lunix -cclib "$(PTHREAD_LINK)"

join.cmxa: $(THREAD_OBJS:.cmo=.cmx) $(JOIN_OBJS:.cmo=.cmx)
	$(CAMLOPT) -nojoin -a -o join.cmxa  \
	../unix/unix.cmx ../unix/unixLabels.cmx \
	$(THREAD_OBJS:.cmo=.cmx) $(JOIN_OBJS:.cmo=.cmx) \
         -cclib -lthreadsnat -cclib -ljoinnat -cclib -lunix -cclib "$(PTHREAD_LINK)"

$(THREAD_OBJS:.cmo=.cmx) $(JOIN_OBJS:.cmo=.cmx): ../../ocamlopt

thread.ml: thread_posix.ml
	ln -s thread_posix.ml thread.ml

join_debug.ml: ../threads/join_debug.ml
	cp -f ../threads/join_debug.ml . && chmod -w join_debug.ml

join_debug.mli: ../threads/join_debug.mli
	cp -f ../threads/join_debug.mli . && chmod -w join_debug.mli

join_misc.ml: ../threads/join_misc.ml
	cp -f ../threads/join_misc.ml . && chmod -w join_misc.ml

join_misc.mli: ../threads/join_misc.mli
	cp -f ../threads/join_misc.mli . && chmod -w join_misc.mli

join_link.ml: ../threads/join_link.ml
	cp -f ../threads/join_link.ml . && chmod -w join_link.ml

join_link.mli: ../threads/join_link.mli
	cp -f ../threads/join_link.mli . && chmod -w join_link.mli

join_port.ml: ../threads/join_port.ml
	cp -f ../threads/join_port.ml . && chmod -w join_port.ml

join_port.mli: ../threads/join_port.mli
	cp -f ../threads/join_port.mli . && chmod -w join_port.mli

join_hash.ml: ../threads/join_hash.ml
	cp -f ../threads/join_hash.ml . && chmod -w join_hash.ml

join_hash.mli: ../threads/join_hash.mli
	cp -f ../threads/join_hash.mli . && chmod -w join_hash.mli

join_set.ml: ../threads/join_set.ml
	cp -f ../threads/join_set.ml . && chmod -w join_set.ml

join_set.mli: ../threads/join_set.mli
	cp -f ../threads/join_set.mli . && chmod -w join_set.mli

join_queue.ml: ../threads/join_queue.ml
	cp -f ../threads/join_queue.ml . && chmod -w join_queue.ml

join_queue.mli: ../threads/join_queue.mli
	cp -f ../threads/join_queue.mli . && chmod -w join_queue.mli

join_scheduler.ml: ../threads/join_scheduler.ml
	cp -f ../threads/join_scheduler.ml . && chmod -w join_scheduler.ml

join_scheduler.mli: ../threads/join_scheduler.mli
	cp -f ../threads/join_scheduler.mli . && chmod -w join_scheduler.mli

join_types.mli: ../threads/join_types.mli
	cp -f ../threads/join_types.mli . && chmod -w join_types.mli

join.ml: ../threads/join.ml
	cp -f ../threads/join.ml . && chmod -w join.ml

join.mli: ../threads/join.mli
	cp -f ../threads/join.mli . && chmod -w join.mli

join_test.ml: ../threads/join_test.ml
	cp -f ../threads/join_test.ml . && chmod -w join_test.ml

join_test.mli: ../threads/join_test.mli
	cp -f ../threads/join_test.mli . && chmod -w join_test.mli

ns.jml: ../threads/ns.jml
	cp -f ../threads/ns.jml . && chmod -w ns.jml

ns.mli: ../threads/ns.mli
	cp -f ../threads/ns.mli . && chmod -w ns.mli

join_message.ml: ../threads/join_message.ml
	cp -f ../threads/join_message.ml . && chmod -w join_message.ml

join_message.mli: ../threads/join_message.mli
	cp -f ../threads/join_message.mli . && chmod -w join_message.mli

join_space.ml: ../threads/join_space.ml
	cp -f ../threads/join_space.ml . && chmod -w join_space.ml

join_space.mli: ../threads/join_space.mli
	cp -f ../threads/join_space.mli . && chmod -w join_space.mli

join.c: ../threads/join.c
	cp -f ../threads/join.c . && chmod -w join.c
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
	cp join.cmi ns.cmi join.cma $(LIBDIR)/threads
	rm -f $(LIBDIR)/threads/stdlib.cma
	cp thread.mli mutex.mli condition.mli event.mli threadUnix.mli $(LIBDIR)
#For join lib and friends
	if test -f dlljoin.so; then cp dlljoin.so $(STUBLIBDIR)/dlljoin.so; fi
	cp libjoin.a $(LIBDIR)/libjoin.a
	cd $(LIBDIR); $(RANLIB) libjoin.a
	cp join.cmi ns.cmi join_test.cmi join.cma $(LIBDIR)/threads


installopt:
	cp libthreadsnat.a $(LIBDIR)/libthreadsnat.a
	cd $(LIBDIR); $(RANLIB) libthreadsnat.a
	cp $(THREAD_OBJS:.cmo=.cmx) threads.cmxa threads.a $(LIBDIR)/threads
	cd $(LIBDIR)/threads; $(RANLIB) threads.a
#For join lib and friends
	cp libjoinnat.a $(LIBDIR)/libjoinnat.a
	cd $(LIBDIR); $(RANLIB) libjoinnat.a
	cp join.cmxa join.a $(LIBDIR)/threads
	cd $(LIBDIR)/threads; $(RANLIB) join.a

.SUFFIXES: .jml .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -nojoin -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -nojoin -c -g $(COMPFLAGS) $(OCAMLPP) $<

.jml.cmo:
	$(JCC) -c -g $(COMPFLAGS) $(OCAMLPP) -impl $<

.ml.cmx:
	$(CAMLOPT) -nojoin -c $(COMPFLAGS) $(OCAMLPP) $<

.jml.cmx:
	$(JCOPT) -c $(COMPFLAGS) $(OCAMLPP) -impl $<

depend: $(GENFILES)
	gcc -MM -I../../byterun *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend
	cp ns.jml ns.ml 
	../../boot/ocamlrun ../../tools/ocamldep -join ns.ml >> .depend
	/bin/rm -f ns.ml

include .depend
