include ../../config/Makefile

CAMLC=../../boot/ocamlrun ../../boot/ocamlc -I ../../stdlib -I ../unix
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib -I ../unix
CPP=/lib/cpp -P -traditional

BYTECODE_C_OBJS=posix_b.o
NATIVECODE_C_OBJS=posix_n.o

THREAD_OBJS=thread.cmo condition.cmo event.cmo threadUnix.cmo

GENFILES=thread.ml

all: libthreads.a threads.cma

allopt: libthreadsnat.a threads.cmxa

libthreads.a: $(BYTECODE_C_OBJS)
	rm -f libthreads.a
	ar rc libthreads.a $(BYTECODE_C_OBJS)

posix_b.o: posix.c
	$(BYTECC) -O -I../../byterun $(BYTECCCOMPOPTS) -c posix.c
	mv posix.o posix_b.o

libthreadsnat.a: $(NATIVECODE_C_OBJS)
	rm -f libthreadsnat.a
	ar rc libthreadsnat.a $(NATIVECODE_C_OBJS)

posix_n.o: posix.c
	$(NATIVECC) -O -I ../../asmrun -I../../byterun $(NATIVECCCOMPOPTS) -DNATIVE_CODE -c posix.c
	mv posix.o posix_n.o

threads.cma: $(THREAD_OBJS)
	$(CAMLC) -a -o threads.cma $(THREAD_OBJS)

threads.cmxa: $(THREAD_OBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o threads.cmxa $(THREAD_OBJS:.cmo=.cmx)

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f libthreads.a *.o *.cm*
	rm -f $(GENFILES)

install:
	cp libthreads.a $(LIBDIR)/libthreads.a
	if test -d $(LIBDIR)/threads; then :; else mkdir $(LIBDIR)/threads; fi
	cp thread.cmi mutex.cmi condition.cmi event.cmi threadUnix.cmi threads.cma stdlib.cma $(LIBDIR)/threads

installopt:

.SUFFIXES: .ml .mli .mlp .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $*.ml
	$(CPP) $(CPPFLAGS) $*.mlp > $*.ml
	@chmod -w $*.ml

thread.ml: thread.mlp

depend:
	gcc -MM -I../../byterun *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
