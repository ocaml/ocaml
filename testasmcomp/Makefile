include ../Makefile.config

CAMLC=../boot/camlrun ../boot/camlc -I ../boot
COMPFLAGS=$(INCLUDES)
LINKFLAGS=
CAMLYACC=../boot/camlyacc
YACCFLAGS=
CAMLLEX=../boot/camlrun ../boot/camllex
CAMLDEP=../tools/camldep
DEPFLAGS=$(INCLUDES)
CAMLRUN=../boot/camlrun

CODEGEN=./codegen
CFLAGS=-g

PROGS=fib.out tak.out quicksort.out quicksort2.out soli.out integr.out

all: codegen $(PROGS)

INCLUDES=-I ../utils -I ../typing -I ../asmcomp

OTHEROBJS=../utils/misc.cmo ../utils/tbl.cmo \
  ../utils/clflags.cmo ../utils/config.cmo \
  ../parsing/location.cmo \
  ../typing/ident.cmo ../typing/path.cmo ../typing/subst.cmo \
  ../typing/predef.cmo ../typing/datarepr.cmo ../typing/env.cmo \
  ../bytecomp/lambda.cmo \
  ../asmcomp/arch.cmo ../asmcomp/cmm.cmo ../asmcomp/printcmm.cmo \
  ../asmcomp/clambda.cmo ../asmcomp/compilenv.cmo \
  ../asmcomp/reg.cmo ../asmcomp/mach.cmo ../asmcomp/proc.cmo \
  ../asmcomp/closure.cmo ../asmcomp/cmmgen.cmo \
  ../asmcomp/printmach.cmo ../asmcomp/selection.cmo \
  ../asmcomp/liveness.cmo ../asmcomp/spill.cmo ../asmcomp/split.cmo \
  ../asmcomp/interf.cmo ../asmcomp/coloring.cmo ../asmcomp/reload.cmo \
  ../asmcomp/linearize.cmo ../asmcomp/printlinear.cmo ../asmcomp/emitaux.cmo \
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

.SUFFIXES:
.SUFFIXES: .cmm .c .o .asm .ml .mli .cmo .cmi

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.cmm.o:
	$(CAMLRUN) $(CODEGEN) $*.cmm > $*.s
	$(AS) $(ASFLAGS) -o $*.o $*.s

.asm.o:
	$(AS) $(ASFLAGS) -o $*.o $*.asm

clean::
	rm -f *.out *.cm[io] *.s *.o *~

fib.o tak.o quicksort.o quicksort2.o integr.o soli.o: $(CODEGEN)

depend: beforedepend
	$(CAMLDEP) $(DEPFLAGS) *.mli *.ml > .depend

include .depend
