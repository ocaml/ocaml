# Makefile for the ndbm library

include ../../config/Makefile

# Compilation optiosn
CC=$(BYTECC)
CAMLC=../../boot/cslrun ../../boot/cslc -I ../../boot
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS)
COBJS=dbm.o
all: libmldbm.a dbm.cmi dbm.cma


libmldbm.a: $(COBJS)
	rm -rf libmldbm.a
	ar rc libmldbm.a $(COBJS)
	$(RANLIB) libmldbm.a

dbm.cma: dbm.cmo
	$(CAMLC) -a -o dbm.cma dbm.cmo

clean:
	rm -f *.cm*

realclean: clean
	rm -rf *.a *.o

install:
	cp libmldbm.a $(LIBDIR)/libmldbm.a
	cd $(LIBDIR); $(RANLIB) libmldbm.a
	cp dbm.cma dbm.cmi $(LIBDIR)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

depend:
	../../tools/csldep *.mli *.ml >> .depend

include .depend
