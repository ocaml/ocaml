# Makefile for the ndbm library

include ../../config/Makefile

# Compilation optiosn
CC=$(BYTECC) -g
CAMLC=../../boot/ocamlrun ../../boot/ocamlc -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS)
COBJS=dbstubs.o

all: libmldb.a db.cmi db.cma

allopt: libmldb.a db.cmi db.cmxa

libmldb.a: $(COBJS)
	rm -rf libmldb.a
	ar rc libmldb.a $(COBJS)
	$(RANLIB) libmldb.a

db.cma: db.cmo
	$(CAMLC) -a -o db.cma db.cmo

db.cmxa: db.cmx
	$(CAMLOPT) -a -o db.cmxa db.cmx

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o

install:
	cp libmldb.a $(LIBDIR)/libmldb.a
	cd $(LIBDIR); $(RANLIB) libmldb.a
	cp db.cma db.cmi db.mli $(LIBDIR)

installopt:
	cp db.cmx db.cmxa db.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) db.a

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
