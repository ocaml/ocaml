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

include ../config/Makefile

CAMLC=../boot/ocamlrun ../boot/ocamlc -I ../boot
COMPFLAGS=$(INCLUDES) -g
LINKFLAGS=-g
CAMLYACC=../boot/ocamlyacc
YACCFLAGS=
CAMLLEX=../boot/ocamlrun ../boot/ocamllex
CAMLDEP=../boot/ocamlrun ../tools/ocamldep
DEPFLAGS=$(INCLUDES)
CAMLRUN=../boot/ocamlrun

CODEGEN=./codegen
CC=$(NATIVECC)
CFLAGS=$(NATIVECCCOMPOPTS) -g

PROGS=fib.out tak.out quicksort.out quicksort2.out soli.out integr.out \
      arith.out checkbound.out

all: codegen $(PROGS)

INCLUDES=-I ../utils -I ../typing -I ../asmcomp

OTHEROBJS=../utils/misc.cmo ../utils/config.cmo ../utils/tbl.cmo \
  ../utils/clflags.cmo ../utils/ccomp.cmo \
  ../utils/config.cmo ../utils/clflags.cmo ../utils/warnings.cmo \
  ../utils/consistbl.cmo \
  ../parsing/linenum.cmo ../parsing/location.cmo \
  ../typing/ident.cmo ../typing/path.cmo ../typing/types.cmo \
  ../typing/btype.cmo ../typing/subst.cmo ../typing/primitive.cmo \
  ../typing/predef.cmo ../typing/datarepr.cmo ../typing/env.cmo \
  ../bytecomp/lambda.cmo ../bytecomp/switch.cmo \
  ../asmcomp/arch.cmo ../asmcomp/cmm.cmo ../asmcomp/printcmm.cmo \
  ../asmcomp/clambda.cmo ../asmcomp/compilenv.cmo \
  ../asmcomp/reg.cmo ../asmcomp/mach.cmo ../asmcomp/proc.cmo \
  ../asmcomp/closure.cmo ../asmcomp/cmmgen.cmo \
  ../asmcomp/printmach.cmo \
  ../asmcomp/selectgen.cmo ../asmcomp/selection.cmo ../asmcomp/comballoc.cmo \
  ../asmcomp/liveness.cmo ../asmcomp/spill.cmo ../asmcomp/split.cmo \
  ../asmcomp/interf.cmo ../asmcomp/coloring.cmo \
  ../asmcomp/reloadgen.cmo ../asmcomp/reload.cmo \
  ../asmcomp/linearize.cmo ../asmcomp/schedgen.cmo ../asmcomp/scheduling.cmo \
  ../asmcomp/printlinear.cmo ../asmcomp/emitaux.cmo \
  ../asmcomp/emit.cmo ../asmcomp/asmgen.cmo

OBJS=parsecmmaux.cmo parsecmm.cmo lexcmm.cmo main.cmo

codegen: $(OTHEROBJS) $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o codegen $(OTHEROBJS) $(OBJS)
clean::
	rm -f codegen

# The parser

parsecmm.mli parsecmm.ml: parsecmm.mly
	$(CAMLYACC) $(YACCFLAGS) parsecmm.mly

clean::
	rm -f parsecmm.mli parsecmm.ml parsecmm.output

beforedepend:: parsecmm.mli parsecmm.ml

# The lexer

lexcmm.ml: lexcmm.mll
	$(CAMLLEX) lexcmm.mll

clean::
	rm -f lexcmm.ml

beforedepend:: lexcmm.ml

# The test programs

$(PROGS:.out=.o): codegen

fib.out: main.c fib.o $(ARCH).o
	$(CC) $(CFLAGS) -o fib.out -DINT_INT -DFUN=fib main.c fib.o $(ARCH).o

tak.out: main.c tak.o $(ARCH).o
	$(CC) $(CFLAGS) -o tak.out -DUNIT_INT -DFUN=takmain main.c tak.o $(ARCH).o

quicksort.out: main.c quicksort.o $(ARCH).o
	$(CC) $(CFLAGS) -o quicksort.out -DSORT -DFUN=quicksort main.c quicksort.o $(ARCH).o

quicksort2.out: main.c quicksort2.o $(ARCH).o
	$(CC) $(CFLAGS) -o quicksort2.out -DSORT -DFUN=quicksort main.c quicksort2.o $(ARCH).o

soli.out: main.c soli.o $(ARCH).o
	$(CC) $(CFLAGS) -o soli.out -DUNIT_INT -DFUN=solitaire main.c soli.o $(ARCH).o

integr.out: main.c integr.o $(ARCH).o
	$(CC) $(CFLAGS) -o integr.out -DINT_FLOAT -DFUN=test main.c integr.o $(ARCH).o

tagged-fib.out: main.c tagged-fib.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-fib.out -DINT_INT -DFUN=fib main.c tagged-fib.o $(ARCH).o

tagged-tak.out: main.c tagged-tak.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-tak.out -DUNIT_INT -DFUN=takmain main.c tagged-tak.o $(ARCH).o

tagged-quicksort.out: main.c tagged-quicksort.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-quicksort.out -DSORT -DFUN=quicksort main.c tagged-quicksort.o $(ARCH).o

tagged-integr.out: main.c tagged-integr.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-integr.out -DINT_FLOAT -DFUN=test main.c tagged-integr.o $(ARCH).o

arith.out: mainarith.c arith.o $(ARCH).o
	$(CC) $(CFLAGS) -o arith.out mainarith.c arith.o $(ARCH).o

checkbound.out: main.c checkbound.o $(ARCH).o
	$(CC) $(CFLAGS) -o checkbound.out -DCHECKBOUND main.c checkbound.o $(ARCH).o

# The runtime environment

power.o: power-$(SYSTEM).o
	cp power-$(SYSTEM).o power.o

.SUFFIXES:
.SUFFIXES: .cmm .c .o .S .ml .mli .cmo .cmi .s

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.cmm.o:
	$(CAMLRUN) $(CODEGEN) $*.cmm > $*.s
	$(AS) $(ASFLAGS) -o $*.o $*.s

.S.o:
	$(ASPP) $(ASPPFLAGS) -o $*.o $*.S

.s.o:
	$(ASPP) $(ASPPFLAGS) -o $*.o $*.s

clean::
	rm -f *.out *.cm[io] *.s *.o *~

$(PROGS:.out=.o): $(CODEGEN)

depend: beforedepend
	$(CAMLDEP) $(DEPFLAGS) *.mli *.ml > .depend

include .depend
