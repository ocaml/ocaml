# Makefile for the str library

include ../../config/Makefile

# Compilation options
CC=$(BYTECC)
CFLAGS=-O -I$(REGEXLIB) -I../../byterun $(BYTECCCOMPOPTS)
CAMLC=../../boot/ocamlrun ../../boot/ocamlc -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib
REGEXLIB=regex-0.12
COBJS=strstubs.o $(REGEXLIB)/regex.o

all: libstr.a str.cmi str.cma

allopt: libstr.a str.cmi str.cmxa

libstr.a: $(COBJS)
	rm -f libstr.a
	ar rc libstr.a $(COBJS)
	$(RANLIB) libstr.a

str.cma: str.cmo
	$(CAMLC) -a -o str.cma str.cmo

str.cmxa: str.cmx
	$(CAMLOPT) -a -o str.cmxa str.cmx

$(REGEXLIB)/regex.o: $(REGEXLIB)/regex.c $(REGEXLIB)/regex.h
	cd $(REGEXLIB); CC="$(CC)" sh configure; $(MAKE)

str.cmx: ../../ocamlopt

clean:
	rm -f *.cm*

realclean: clean
	rm -f *.a *.o
	cd $(REGEXLIB); if test -f Makefile; then $(MAKE) distclean; else exit 0; fi

install:
	cp libstr.a $(LIBDIR)/libstr.a
	cd $(LIBDIR); $(RANLIB) libstr.a
	cp str.cma str.cmi $(LIBDIR)

installopt:
	cp str.cmx str.cmxa str.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) str.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../tools/ocamldep *.mli *.ml >> .depend

include .depend
