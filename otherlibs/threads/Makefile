include ../../Makefile.config

# Compilation options
CC=$(BYTECC)
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS)
CAMLC=../../boot/cslrun ../../boot/cslc -I ../../boot -I ../unix

C_OBJS=scheduler.o
CAML_OBJS=thread.cmo threadIO.cmo mutex.cmo condition.cmo event.cmo

all: libthreads.a threads.cma

allopt:

libthreads.a: $(C_OBJS)
	rm -f libthreads.a
	ar rc libthreads.a $(C_OBJS)
	$(RANLIB) libthreads.a

threads.cma: $(CAML_OBJS)
	$(CAMLC) -a -o threads.cma $(CAML_OBJS)

clean:
	rm -f libthreads.a *.o *.cm*

realclean: clean

install:
	cp libthreads.a $(LIBDIR)/libthreads.a
	cd $(LIBDIR); $(RANLIB) libthreads.a
	cp *.cmi threads.cma $(LIBDIR)

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
	../../tools/csldep *.mli *.ml >> .depend

include .depend
