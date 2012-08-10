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

# $Id$

include ../../config/Makefile
NOJOIN=-nojoin
CAMLC=../../ocamlcomp.sh $(NOJOIN) -I ../unix
CAMLOPT=../../ocamlcompopt.sh $(NOJOIN) -I ../unix
JCC=../../ocamlcomp.sh -I ../unix
JCOPT=../../ocamlcompopt.sh -I ../unix
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
COMPFLAGS=-warn-error A -g
OCAMLPP=-pp '../../tools/remove_DEBUG'


BYTECODE_JOIN_C_OBJS=join_b.o
NATIVECODE_JOIN_C_OBJS=join_n.o

JOIN_OBJS=join_debug.cmo join_misc.cmo join_link.cmo join_hash.cmo\
 join_set.cmo join_queue.cmo join_extern.cmo join_scheduler.cmo join_port.cmo\
 join_message.cmo join_space.cmo join_prim.cmo join_test.cmo

BYTECODE_C_OBJS=st_stubs_b.o $(BYTECODE_JOIN_C_OBJS)
NATIVECODE_C_OBJS=st_stubs_n.o $(NATIVECODE_JOIN_C_OBJS)
CAML_THREAD_OBJS=thread.cmo mutex.cmo condition.cmo event.cmo threadUnix.cmo 
THREAD_OBJS= $(CAML_THREAD_OBJS) $(JOIN_OBJS)

GENFILES=join.c join_types.mli\
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
 join_prim.ml join_prim.mli\
 join_test.mli join_test.ml

all: libthreads.a threads.cma 


allopt: libthreadsnat.a threads.cmxa 

libthreads.a: $(BYTECODE_C_OBJS)
	$(MKLIB) -o threads $(BYTECODE_C_OBJS)

st_stubs_b.o: st_stubs.c st_posix.h
	$(BYTECC) -O -I../../byterun $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS) \
	   -c st_stubs.c
	mv st_stubs.o st_stubs_b.o

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

st_stubs_n.o: st_stubs.c st_posix.h
	$(NATIVECC) -O -I../../asmrun -I../../byterun $(NATIVECCCOMPOPTS) $(SHAREDCCCOMPOPTS) -DNATIVE_CODE -DTARGET_$(ARCH) -DSYS_$(SYSTEM) -c st_stubs.c
	mv st_stubs.o st_stubs_n.o

join_n.o: join.c
	$(NATIVECC) -DCAML_NAME_SPACE -O -I../../asmrun -I../../byterun $(NATIVECCCOMPOPTS) $(SHAREDCCCOMPOPTS) -DNATIVE_CODE -DTARGET_$(ARCH) -DSYS_$(SYSTEM) -c join.c
	mv join.o join_n.o

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


$(THREAD_OBJS:.cmo=.cmx) $(JOIN_OBJS:.cmo=.cmx): ../../ocamlopt

join_debug.ml: ../threads/join_debug.ml
	ln -s ../threads/join_debug.ml .

join_debug.mli: ../threads/join_debug.mli
	ln -s ../threads/join_debug.mli .

join_misc.ml: ../threads/join_misc.ml
	ln -s ../threads/join_misc.ml .

join_misc.mli: ../threads/join_misc.mli
	ln -s ../threads/join_misc.mli .

join_link.ml: ../threads/join_link.ml
	ln -s ../threads/join_link.ml .

join_link.mli: ../threads/join_link.mli
	ln -s ../threads/join_link.mli .

join_port.ml: ../threads/join_port.ml
	ln -s ../threads/join_port.ml .

join_port.mli: ../threads/join_port.mli
	ln -s ../threads/join_port.mli .

join_hash.ml: ../threads/join_hash.ml
	ln -s ../threads/join_hash.ml .

join_hash.mli: ../threads/join_hash.mli
	ln -s ../threads/join_hash.mli .

join_set.ml: ../threads/join_set.ml
	ln -s ../threads/join_set.ml .

join_set.mli: ../threads/join_set.mli
	ln -s ../threads/join_set.mli .

join_queue.ml: ../threads/join_queue.ml
	ln -s ../threads/join_queue.ml .

join_queue.mli: ../threads/join_queue.mli
	ln -s ../threads/join_queue.mli .

join_scheduler.ml: ../threads/join_scheduler.ml
	ln -s ../threads/join_scheduler.ml .

join_scheduler.mli: ../threads/join_scheduler.mli
	ln -s ../threads/join_scheduler.mli .

join_types.mli: ../threads/join_types.mli
	ln -s ../threads/join_types.mli .

join_prim.ml: ../threads/join_prim.ml
	ln -s ../threads/join_prim.ml .

join_prim.mli: ../threads/join_prim.mli
	ln -s ../threads/join_prim.mli .

join_test.ml: ../threads/join_test.ml
	ln -s ../threads/join_test.ml .

join_test.mli: ../threads/join_test.mli
	ln -s ../threads/join_test.mli .

join_message.ml: ../threads/join_message.ml
	ln -s ../threads/join_message.ml .

join_message.mli: ../threads/join_message.mli
	ln -s ../threads/join_message.mli .

join_space.ml: ../threads/join_space.ml
	ln -s ../threads/join_space.ml .

join_space.mli: ../threads/join_space.mli
	ln -s ../threads/join_space.mli .

join.c: ../threads/join.c
	ln -s ../threads/join.c .

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.o *.a *.so

install:
	if test -f dllthreads.so; then cp dllthreads.so $(STUBLIBDIR)/dllthreads.so; fi
	cp libthreads.a $(LIBDIR)/libthreads.a
	cd $(LIBDIR); $(RANLIB) libthreads.a
	if test -d $(LIBDIR)/threads; then :; else mkdir $(LIBDIR)/threads; fi
	cp thread.mli mutex.mli condition.mli\
  event.mli threadUnix.mli $(LIBDIR)/threads
	cp join_prim.cmi join_test.cmi $(LIBDIR)/threads
	cp $(CAML_THREAD_OBJS:.cmo=.cmi) threads.cma $(LIBDIR)/threads
	cp threads.h $(LIBDIR)/caml/threads.h

installopt:
	cp libthreadsnat.a $(LIBDIR)/libthreadsnat.a
	cd $(LIBDIR); $(RANLIB) libthreadsnat.a
	cp $(THREAD_OBJS:.cmo=.cmx) threads.cmxa threads.a $(LIBDIR)/threads
	cd $(LIBDIR)/threads; $(RANLIB) threads.a


.SUFFIXES: .jml .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -g -c $(COMPFLAGS) $(OCAMLPP) $<

.jml.cmo:
	$(JCC) -c $(COMPFLAGS) $(OCAMLPP) -impl $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $(OCAMLPP) $<

.jml.cmx:
	$(JCOPT) -c $(COMPFLAGS) $(OCAMLPP) -impl $<

depend: $(GENFILES)
	-gcc -MM -I../../byterun *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep -nojoin *.mli *.ml >> .depend

include .depend
