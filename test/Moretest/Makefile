include ../../config/Makefile

CAMLC=../../boot/cslrun ../../cslc -I ../../stdlib
CAMLOPT=../../boot/cslrun ../../cslopt -I ../../stdlib
OPTFLAGS=-S
CAMLDEP=../../boot/cslrun ../../tools/csldep
CAMLRUN=../../byterun/cslrun
CODERUNPARAMS=CAMLRUNPARAM='o=100'

callback.byt: callback.cmo callbackprim.o
	$(CAMLC) -o callback.byt -custom callback.cmo callbackprim.o
callback.out: callback.cmx callbackprim.o
	$(CAMLOPT) -o callback.out callback.cmx callbackprim.o

manyargs.byt: manyargs.cmo manyargsprim.o
	$(CAMLC) -o manyargs.byt -custom manyargs.cmo manyargsprim.o
manyargs.out: manyargs.cmx manyargsprim.o
	$(CAMLOPT) -o manyargs.out manyargs.cmx manyargsprim.o

# Common rules

.SUFFIXES:
.SUFFIXES: .mli .ml .cmi .cmo .cmx .byt .fast.byt .out .fast.out .c .o

.ml.byt:
	$(CAMLC) -o $*.byt $<

.ml.fast.byt:
	cp $*.ml $*_fast.ml
	$(CAMLC) -unsafe -o $*.fast.byt $*_fast.ml
	rm -f $*_fast.ml

.ml.out:
	$(CAMLOPT) $(OPTFLAGS) -o $*.out $<

.ml.fast.out:
	cp $*.ml $*_fast.ml
	$(CAMLOPT) $(OPTFLAGS) -unsafe -o $*.fast.out $*_fast.ml
	rm -f $*_fast.ml

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmo:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) $(OPTFLAGS) -c $<

.c.o:
	$(NATIVECC) -I../../byterun -c $<

clean::
	rm -f *.byt *.out
	rm -f *.cm[iox] *.[os]
	rm -f *~

# Dependencies

depend:
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

