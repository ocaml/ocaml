#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id$

include ../../config/Makefile

CAMLC=../../boot/ocamlrun ../../ocamlc -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib
OPTFLAGS=-S
CAMLDEP=../../boot/ocamlrun ../../tools/ocamldep
CAMLRUN=../../byterun/ocamlrun
CODERUNPARAMS=OCAMLRUNPARAM='o=100'

callback.byt: callback.cmo callbackprim.o
	$(CAMLC) -o callback.byt -custom callback.cmo callbackprim.o ../../otherlibs/unix/libunix.a
callback.out: callback.cmx callbackprim.o
	$(CAMLOPT) -o callback.out callback.cmx callbackprim.o ../../otherlibs/unix/libunix.a

manyargs.byt: manyargs.cmo manyargsprim.o
	$(CAMLC) -o manyargs.byt -custom manyargs.cmo manyargsprim.o
manyargs.out: manyargs.cmx manyargsprim.o
	$(CAMLOPT) -o manyargs.out manyargs.cmx manyargsprim.o

multdef.out: multdef.cmx usemultdef.cmx
	$(CAMLOPT) -o multdef.out multdef.cmx usemultdef.cmx

cm.byt: cmcaml.ml cmstub.c cmmain.c
	$(CAMLC) -custom -o cm.byt cmcaml.ml cmstub.c cmmain.c

cmlinked.out: cmcaml.ml cmstub.c cmmain.c
	$(CAMLC) -output-obj -o cm.o cmcaml.ml
	$(BYTECC) -g -o cmlinked.out cm.o -I../../byterun -DNO_BYTECODE_FILE cmstub.c cmmain.c ../../byterun/libcamlrun.a $(CCLIBS)

cm.out: cmcaml.ml cmstub.c cmmain.c
	$(CAMLOPT) -output-obj -o cm.o cmcaml.ml
	$(NATIVECC) -g -o cm.out cm.o -I$(LIBDIR) -DNO_BYTECODE_FILE cmstub.c cmmain.c ../../asmrun/libasmrun.a $(CCLIBS)

bigarrays.byt: ../../otherlibs/bigarray/bigarray.cma \
               ../../otherlibs/bigarray/libbigarray.a bigarrays.ml
	$(CAMLC) -o bigarrays.byt \
                -I ../../otherlibs/bigarray \
                -I ../../otherlibs/unix \
                unix.cma bigarray.cma bigarrays.ml \
                -ccopt -L../../otherlibs/bigarray \
                -ccopt -L../../otherlibs/unix

bigarrays.out: ../../otherlibs/bigarray/bigarray.cmxa \
               ../../otherlibs/bigarray/libbigarray.a bigarrays.ml
	$(CAMLOPT) $(OPTFLAGS) -o bigarrays.out \
                -I ../../otherlibs/bigarray \
                -I ../../otherlibs/unix \
                unix.cmxa bigarray.cmxa bigarrays.ml \
                -ccopt -L../../otherlibs/bigarray \
                -ccopt -L../../otherlibs/unix

bigarrf.byt: bigarrf.o bigarrfstub.o \
               ../../otherlibs/bigarray/bigarray.cma \
               ../../otherlibs/bigarray/libbigarray.a bigarrfml.ml
	$(CAMLC) -custom -o bigarrf.byt \
                -I ../../otherlibs/bigarray \
                -I ../../otherlibs/unix \
                unix.cma bigarray.cma bigarrf.ml \
                bigarrf.o bigarrfstub.o \
                -ccopt -L../../otherlibs/bigarray \
                -ccopt -L../../otherlibs/unix \
                ../../byterun/libcamlrun.a -cclib -lg2c

bigarrf.out: bigarrf.o bigarrfstub.o \
               ../../otherlibs/bigarray/bigarray.cma \
               ../../otherlibs/bigarray/libbigarray.a bigarrfml.ml
	$(CAMLOPT) $(OPTFLAGS) -o bigarrf.out \
                -I ../../otherlibs/bigarray \
                -I ../../otherlibs/unix \
                unix.cma bigarray.cma bigarrf.ml \
                bigarrf.o bigarrfstub.o \
                -ccopt -L../../otherlibs/bigarray \
                -ccopt -L../../otherlibs/unix \
                ../../byterun/libcamlrun.a -cclib -lg2c

bigarrf.o: bigarrf.f
	g77 -c bigarrf.f

bigarrfstub.o: bigarrfstub.c
	$(NATIVECC) $(NATIVECCCOMPOPTS) -I../../byterun -I../../otherlibs/bigarray -c bigarrfstub.c

fftba.byt: fftba.ml
	$(CAMLC) -o fftba.byt -I ../../otherlibs/bigarray \
          bigarray.cma fftba.ml \
          -ccopt -L../../otherlibs/bigarray

fftba.out: fftba.ml
	$(CAMLOPT) $(OPTFLAGS) -o fftba.out -I ../../otherlibs/bigarray \
          bigarray.cmxa fftba.ml \
          -ccopt -L../../otherlibs/bigarray

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

