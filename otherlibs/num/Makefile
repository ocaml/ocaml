# Makefile for the "num" (exact rational arithmetic) library

include ../../config/Makefile

# Compilation options
CC=$(BYTECC)
CFLAGS=-O -I./bignum/h -I../../byterun $(BYTECCCOMPOPTS)
CAMLC=../../boot/ocamlrun ../../boot/ocamlc -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib

CAMLOBJS=int_misc.cmo string_misc.cmo nat.cmo big_int.cmo arith_flags.cmo \
  ratio.cmo num.cmo arith_status.cmo

CMIFILES=big_int.cmi nat.cmi num.cmi ratio.cmi arith_status.cmi

COBJS=nat_stubs.o

all: libnums.a nums.cma $(CMIFILES)

allopt: libnums.a nums.cmxa $(CMIFILES)

nums.cma: $(CAMLOBJS)
	$(CAMLC) -a -o nums.cma $(CAMLOBJS)

nums.cmxa: $(CAMLOBJS:.cmo=.cmx)
	$(CAMLOPT) -a -o nums.cmxa $(CAMLOBJS:.cmo=.cmx)

libnums.a: bignum/libbignum.a $(COBJS)
	cp bignum/libbignum.a libnums.a
	ar r libnums.a $(COBJS)
	$(RANLIB) libnums.a

bignum/libbignum.a:
	cd bignum; $(MAKE) $(BIGNUM_ARCH) CC="$(CC)"

$(CAMLOBJS:.cmo=.cmx): ../../ocamlopt

install:
	cp libnums.a $(LIBDIR)/libnums.a
	cd $(LIBDIR); $(RANLIB) libnums.a
	cp nums.cma $(CMIFILES) $(LIBDIR)

installopt:
	cp $(CAMLOBJS:.cmo=.cmx) nums.cmxa nums.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) nums.a

clean:
	rm -f *.cm*

realclean: clean
	rm -f *.a *.o
	cd bignum; $(MAKE) scratch
	cd test; $(MAKE) clean

.SUFFIXES: .ml .mli .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

nat_stubs.o: nat.h

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../tools/ocamldep *.mli *.ml >> .depend

include .depend
