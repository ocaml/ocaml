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

CAMLC=../../boot/ocamlrun ../../ocamlc -I ../../stdlib -I ../../otherlibs/unix
BYTEFLAGS=-g
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib -I ../../otherlibs/unix
OPTFLAGS=-S -g
CAMLDEP=../../boot/ocamlrun ../../tools/ocamldep
CAMLRUN=../../byterun/ocamlrun
CODERUNPARAMS=OCAMLRUNPARAM='o=100'

#FORTRANC=g77
#FORTRANLIB=g2c
FORTRANC=gfortran
FORTRANLIB=gfortran

OUTS=backtrace2.bin callback.bin manyargs.bin \
cm.byt cmlinked.bin cm.bin \
bigarrays.bin bigarrf.bin fftba.bin globroots.bin float.bin intext.bin \
printf scanf regexp.byt regexp.bin md5.bin recmod.bin

BROKENS=

all: $(OUTS:.bin=.byt) $(OUTS) recmods

callback.byt: tcallback.cmo callbackprim.o
	$(CAMLC) -o callback.byt \
	  -custom tcallback.cmo callbackprim.o ../../otherlibs/unix/libunix.a
callback.bin: tcallback.cmx callbackprim.o
	$(CAMLOPT) -o tcallback.bin tcallback.cmx \
	  callbackprim.o ../../otherlibs/unix/libunix.a

manyargs.byt: manyargs.cmo manyargsprim.o
	$(CAMLC) -o manyargs.byt -custom manyargs.cmo manyargsprim.o
manyargs.bin: manyargs.cmx manyargsprim.o
	$(CAMLOPT) -inline 0 -o manyargs.bin manyargs.cmx manyargsprim.o

multdef.byt: multdef.cmo usemultdef.cmo
	$(CAMLC) -o multdef.byt multdef.cmo usemultdef.cmo
multdef.bin: multdef.cmx usemultdef.cmx
	$(CAMLOPT) -o multdef.bin multdef.cmx usemultdef.cmx

cm.byt: cmcaml.ml cmstub.c cmmain.c
	$(CAMLC) -custom -o cm.byt cmcaml.ml cmstub.c cmmain.c

cmlinked.bin: cmcaml.ml cmstub.c cmmain.c
	$(CAMLC) -output-obj -o cm.o cmcaml.ml
	$(BYTECC) -g -o cmlinked.bin cm.o \
	  -I../../asmrun -I/usr/local/lib/ocaml \
	  -DNO_BYTECODE_FILE cmstub.c cmmain.c \
	  ../../byterun/libcamlrun.a $(BYTECCLIBS)
cmlinked.byt: cmcaml.ml cmstub.c cmmain.c
	$(CAMLC) -output-obj -o cm.o cmcaml.ml
	$(BYTECC) -g -o cmlinked.byt cm.o  \
	  -I../../byterun -I/usr/local/lib/ocaml \
	  -DNO_BYTECODE_FILE cmstub.c cmmain.c \
	  ../../byterun/libcamlrun.a $(BYTECCLIBS)

cm.bin: cmcaml.ml cmstub.c cmmain.c
	$(CAMLOPT) -output-obj -o cm.o cmcaml.ml
	$(NATIVECC) -g -o cm.bin cm.o -I$(LIBDIR) \
	  -DNO_BYTECODE_FILE cmstub.c cmmain.c \
	  ../../asmrun/libasmrun.a $(NATIVECCLIBS)

bigarrays.byt: ../../otherlibs/bigarray/bigarray.cma \
               ../../otherlibs/bigarray/libbigarray.a bigarrays.ml
	$(CAMLC) -custom -o bigarrays.byt \
                -I ../../otherlibs/bigarray \
                -I ../../otherlibs/unix \
                unix.cma bigarray.cma bigarrays.ml

bigarrays.bin: ../../otherlibs/bigarray/bigarray.cmxa \
               ../../otherlibs/bigarray/libbigarray.a bigarrays.ml
	$(CAMLOPT) $(OPTFLAGS) -o bigarrays.bin \
                -I ../../otherlibs/bigarray \
                -I ../../otherlibs/unix \
                unix.cmxa bigarray.cmxa bigarrays.ml

bigarrf.byt: bigarrf.o bigarrfstub.o \
               ../../otherlibs/bigarray/bigarray.cma \
               ../../otherlibs/bigarray/libbigarray.a bigarrfml.ml
	$(CAMLC) -custom -o bigarrf.byt \
                -I ../../otherlibs/bigarray \
                -I ../../otherlibs/unix \
                unix.cma bigarray.cma bigarrfml.ml \
                bigarrf.o bigarrfstub.o \
                ../../byterun/libcamlrun.a -cclib -l$(FORTRANLIB)

bigarrf.bin: bigarrf.o bigarrfstub.o \
               ../../otherlibs/bigarray/bigarray.cma \
               ../../otherlibs/bigarray/libbigarray.a bigarrfml.ml
	$(CAMLOPT) $(OPTFLAGS) -o bigarrf.bin \
                -I ../../otherlibs/bigarray \
                -I ../../otherlibs/unix \
                unix.cmxa bigarray.cmxa bigarrfml.ml \
                bigarrf.o bigarrfstub.o \
                ../../asmrun/libasmrun.a -cclib -l$(FORTRANLIB)

bigarrf.o: bigarrf.f
	$(FORTRANC) -c bigarrf.f

bigarrfstub.o: bigarrfstub.c
	$(NATIVECC) $(NATIVECCCOMPOPTS) -I../../byterun \
	  -I../../otherlibs/bigarray -c bigarrfstub.c

fftba.byt: fftba.ml
	$(CAMLC) -o fftba.byt -I ../../otherlibs/bigarray \
          bigarray.cma fftba.ml

fftba.bin: fftba.ml
	$(CAMLOPT) $(OPTFLAGS) -o fftba.bin -I ../../otherlibs/bigarray \
          bigarray.cmxa fftba.ml

globroots.byt: globroots.ml globrootsprim.o
	$(CAMLC) -custom -o globroots.byt globroots.ml globrootsprim.o

globroots.bin: globroots.ml globrootsprim.o
	$(CAMLOPT) -o globroots.bin globroots.ml globrootsprim.o

globrootsprim.o: globrootsprim.c
	$(BYTECC) $(BYTECCCOMPOPTS) -I../../byterun -c globrootsprim.c

float.byt: float.cmo
	${CAMLC} -o float.byt float.cmo
float.bin: float.cmx
	${CAMLOPT} -o float.bin float.cmx

intext.byt: intext.cmo intextaux.o
	${CAMLC} -o intext.byt -custom intext.cmo intextaux.o
intext.bin: intext.cmx intextaux.o
	${CAMLOPT} -o intext.bin intext.cmx intextaux.o

tprintf.byt: testing.cmo tprintf.cmo
	${CAMLC} -o tprintf.byt testing.cmo tprintf.cmo
tprintf.bin: testing.cmx tprintf.cmx
	${CAMLOPT} -o tprintf.bin testing.cmx tprintf.cmx

printf: tprintf.byt tprintf.bin
	./tprintf.byt
	./tprintf.bin

tformat.byt: testing.cmo tformat.cmo
	${CAMLC} -o tformat.byt testing.cmo tformat.cmo
tformat.bin: testing.cmx tformat.cmx
	${CAMLOPT} -o tformat.bin testing.cmx tformat.cmx

format: tformat.byt tformat.bin
	./tformat.byt
	./tformat.bin

tbuffer.byt: testing.cmo tbuffer.cmo
	${CAMLC} -o tbuffer.byt testing.cmo tbuffer.cmo
tbuffer.bin: testing.cmx tbuffer.cmx
	${CAMLOPT} -o tbuffer.bin testing.cmx tbuffer.cmx

buffer: tscanf.byt tscanf.bin
	./tbuffer.byt
	./tbuffer.bin

tscanf.byt: testing.cmo tscanf.cmo
	${CAMLC} -o tscanf.byt testing.cmo tscanf.cmo
tscanf.bin: testing.cmx tscanf.cmx
	${CAMLOPT} -o tscanf.bin testing.cmx tscanf.cmx

tscanf2.byt: tscanf2_io.cmo tscanf2_slave.cmo tscanf2_master.cmo
	${CAMLC} -o tscanf2_slave.byt tscanf2_io.cmo tscanf2_slave.cmo
	${CAMLC} -o tscanf2_master.byt unix.cma \
		tscanf2_io.cmo tscanf2_master.cmo
tscanf2.bin: tscanf2_io.cmx tscanf2_slave.cmx tscanf2_master.cmx
	${CAMLOPT} -o tscanf2_slave.bin tscanf2_io.cmx tscanf2_slave.cmx
	${CAMLOPT} -o tscanf2_master.bin unix.cmxa \
		tscanf2_io.cmx tscanf2_master.cmx

scanf: tscanf.byt tscanf.bin tscanf2.byt tscanf2.bin
	./tscanf.byt
	./tscanf.bin
	./tscanf2_master.byt ./tscanf2_slave.byt
	./tscanf2_master.bin ./tscanf2_slave.bin

regexp.byt: ../../otherlibs/str/str.cma regexp.ml
	$(CAMLC) -custom -I ../../otherlibs/str -o regexp.byt str.cma regexp.ml
regexp.bin: ../../otherlibs/str/str.cmxa regexp.ml
	$(CAMLOPT) -I ../../otherlibs/str -o regexp.bin str.cmxa regexp.ml

md5.bin: md5.ml
	$(CAMLOPT) -unsafe -inline 100 -o md5.bin md5.ml

recmod.byt: recmod.cmo
	$(CAMLC) -o recmod.byt recmod.cmo

recmod.bin: recmod.cmx
	$(CAMLOPT) -o recmod.bin recmod.cmx

recmods:
	set -e; \
        for i in recmod/*.ml; do \
          case $$i in \
          *ok.ml) \
            echo "$$i (should succeed)"; \
            $(CAMLC) -c $$i;; \
          *bad.ml) \
            echo "$$i (should fail)"; \
            if $(CAMLC) -c $$i; then exit 2; else :; fi;; \
          esac; \
        done

clean::
	rm -f recmods/*.cm[io]

length.cmo: length.cmi
tlength.cmo: length.cmo

tlength.byt: length.cmo tlength.cmo
	$(CAMLC) -g -o tlength.byt length.cmo tlength.cmo

float_record.cmo: float_record.cmi
tfloat_record.cmo: float_record.cmo
tfloat_record.byt: float_record.cmo tfloat_record.cmo
	$(CAMLC) -g -o tfloat_record.byt float_record.cmo tfloat_record.cmo

# Common rules

.SUFFIXES:
.SUFFIXES: .mli .ml .cmi .cmo .cmx .byt .fast.byt .bin .fast.bin .c .o

.ml.byt:
	$(CAMLC) $(BYTEFLAGS) -o $*.byt $<

.ml.fast.byt:
	cp $*.ml $*_fast.ml
	$(CAMLC) $(BYTEFLAGS) -unsafe -o $*.fast.byt $*_fast.ml
	rm -f $*_fast.ml

.ml.bin:
	$(CAMLOPT) $(OPTFLAGS) -o $*.bin $<

.ml.fast.bin:
	cp $*.ml $*_fast.ml
	$(CAMLOPT) $(OPTFLAGS) -unsafe -o $*.fast.bin $*_fast.ml
	rm -f $*_fast.ml

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmo:
	$(CAMLC) $(BYTEFLAGS) -c -g $<

.ml.cmx:
	$(CAMLOPT) $(OPTFLAGS) -c $<

.c.o:
	$(NATIVECC) $(NATIVECCCOMPOPTS) -I../../byterun -c $<

clean::
	rm -f *.byt *.bin a.out
	rm -f *.cm[iox] *.[os]
	rm -f *~
	rm -f intext.data

# Dependencies

#depend:
#	$(CAMLDEP) *.mli *.ml > .depend

include .depend
