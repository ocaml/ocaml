include ../../config/Makefile

CAMLC=../../boot/ocamlrun ../../ocamlc -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib
OPTFLAGS=-S
CAMLDEP=../../boot/ocamlrun ../../tools/ocamldep
CAMLRUN=../../byterun/ocamlrun
CODERUNPARAMS=CAMLRUNPARAM='o=100'

callback.byt: callback.cmo callbackprim.o
	$(CAMLC) -o callback.byt -custom callback.cmo callbackprim.o
callback.out: callback.cmx callbackprim.o
	$(CAMLOPT) -o callback.out callback.cmx callbackprim.o

manyargs.byt: manyargs.cmo manyargsprim.o
	$(CAMLC) -o manyargs.byt -custom manyargs.cmo manyargsprim.o
manyargs.out: manyargs.cmx manyargsprim.o
	$(CAMLOPT) -o manyargs.out manyargs.cmx manyargsprim.o

multdef.out: multdef.cmx usemultdef.cmx
	$(CAMLOPT) -o multdef.out multdef.cmx usemultdef.cmx

cm.byt:
	$(CAMLC) -custom -o cm.byt cmcaml.ml cmstub.c cmmain.c

cmlinked.out: cmcaml.cmo
	$(CAMLC) -output-obj -o cm.o cmcaml.ml
	$(BYTECC) -g -o cmlinked.out cm.o -I$(LIBDIR) -DNO_BYTECODE_FILE cmstub.c cmmain.c ../../byterun/libcamlrun.a $(CCLIBS)

cm.out:
	$(CAMLOPT) -output-obj -o cm.o cmcaml.ml
	$(NATIVECC) -g -o cm.out cm.o -I$(LIBDIR) -DNO_BYTECODE_FILE cmstub.c cmmain.c ../../asmrun/libasmrun.a $(CCLIBS)

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
	$(NATIVECC) $(NATIVECCCOMPOPTS) -I../../byterun -c $<

clean::
	rm -f *.byt *.out
	rm -f *.cm[iox] *.[os]
	rm -f *~
	rm -f intext.data

# Dependencies

depend:
	$(CAMLDEP) *.mli *.ml > .depend

include .depend

