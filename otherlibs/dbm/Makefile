# Makefile for the ndbm library

include ../../config/Makefile

# Compilation optiosn
CC=$(BYTECC) -g
CAMLC=../../boot/ocamlrun ../../boot/ocamlc -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS)
COBJS=cldbm.o

all: libmldbm.a dbm.cmi dbm.cma

allopt: libmldbm.a dbm.cmi dbm.cmxa

libmldbm.a: $(COBJS)
	rm -rf libmldbm.a
	ar rc libmldbm.a $(COBJS)
	$(RANLIB) libmldbm.a

dbm.cma: dbm.cmo
	$(CAMLC) -a -o dbm.cma dbm.cmo

dbm.cmxa: dbm.cmx
	$(CAMLOPT) -a -o dbm.cmxa dbm.cmx

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o

install:
	cp libmldbm.a $(LIBDIR)/libmldbm.a
	cd $(LIBDIR); $(RANLIB) libmldbm.a
	cp dbm.cma dbm.cmi $(LIBDIR)

installopt:
	cp dbm.cmx dbm.cmxa dbm.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) dbm.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	../../tools/ocamldep *.mli *.ml > .depend

include .depend
