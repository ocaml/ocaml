# Makefile for the "num" (exact rational arithmetic) library

include ../../Makefile.config

# Compilation options
CC=$(BYTECC)
CFLAGS=-O -I./bignum/h -I../../byterun $(BYTECCCOMPOPTS)
CAMLC=../../boot/cslrun ../../boot/cslc -I ../../boot
CAMLOPT=../../boot/cslrun ../../cslopt -I ../../stdlib

CAMLOBJS=int_misc.cmo string_misc.cmo nat.cmo big_int.cmo arith_flags.cmo \
  ratio.cmo num.cmo arith_status.cmo

CAMLINTF=big_int.cmi nat.cmi num.cmi ratio.cmi arith_status.cmi

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
	cd bignum; make $(BIGNUM_ARCH) CC="$(CC)"

$(CAMLOBJS:.cmo=.cmx): ../../cslopt

install:
	cp libnums.a $(LIBDIR)/libnums.a
	cd $(LIBDIR); $(RANLIB) libnums.a
	cp nums.cma $(CAMLINTF) $(LIBDIR)

installopt:
	cp $(CAMLOBJS:.cmo=.cmx) nums.cmxa nums.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) nums.a

clean:
	rm -f *.cm*

realclean: clean
	rm -f *.a *.o
	rm -f nat.ml int_misc.ml
	cd bignum; make scratch
	cd test; make clean

.SUFFIXES: .ml .mli .mlp .cmi .cmo .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.mlp.ml:
	@rm -f $@
	$(CPP) $< > $@
	@chmod a-w $@

int_misc.ml: int_misc.mlp
nat.ml: nat.mlp
nat_stubs.o: nat.h

depend: nat.ml int_misc.ml
	gcc -MM $(CFLAGS) *.c > .depend
	../../tools/csldep *.mli *.ml >> .depend

include .depend
