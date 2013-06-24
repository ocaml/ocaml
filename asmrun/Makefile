#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../LICENSE.      #
#                                                                       #
#########################################################################

include ../config/Makefile

CC=$(NATIVECC)
FLAGS=-I../byterun -DCAML_NAME_SPACE -DNATIVE_CODE \
      -DTARGET_$(ARCH) -DSYS_$(SYSTEM) $(IFLEXDIR)
CFLAGS=$(FLAGS) -O $(NATIVECCCOMPOPTS)
DFLAGS=$(FLAGS) -g -DDEBUG $(NATIVECCCOMPOPTS)
PFLAGS=$(FLAGS) -pg -O -DPROFILING $(NATIVECCPROFOPTS)

COBJS=startup.o main.o fail.o roots.o globroots.o signals.o signals_asm.o \
  misc.o freelist.o major_gc.o minor_gc.o memory.o alloc.o compare.o ints.o \
  floats.o str.o array.o io.o extern.o intern.o hash.o sys.o parsing.o \
  gc_ctrl.o terminfo.o md5.o obj.o lexing.o printexc.o callback.o weak.o \
  compact.o finalise.o custom.o unix.o backtrace.o natdynlink.o debugger.o \
  meta.o dynlink.o

ASMOBJS=$(ARCH).o

OBJS=$(COBJS) $(ASMOBJS)
DOBJS=$(COBJS:.o=.d.o) $(ASMOBJS)
POBJS=$(COBJS:.o=.p.o) $(ASMOBJS:.o=.p.o)

all: libasmrun.a all-$(RUNTIMED) all-$(PROFILING)

libasmrun.a: $(OBJS)
	rm -f libasmrun.a
	ar rc libasmrun.a $(OBJS)
	$(RANLIB) libasmrun.a

all-noruntimed:
.PHONY: all-noruntimed

all-runtimed: libasmrund.a
.PHONY: all-runtimed

libasmrund.a: $(DOBJS)
	rm -f libasmrund.a
	ar rc libasmrund.a $(DOBJS)
	$(RANLIB) libasmrund.a

all-noprof:

all-prof: libasmrunp.a

libasmrunp.a: $(POBJS)
	rm -f libasmrunp.a
	ar rc libasmrunp.a $(POBJS)
	$(RANLIB) libasmrunp.a

install: install-default install-$(RUNTIMED) install-$(PROFILING)

install-default:
	cp libasmrun.a $(LIBDIR)/libasmrun.a
	cd $(LIBDIR); $(RANLIB) libasmrun.a

install-noruntimed:
.PHONY: install-noruntimed

install-runtimed:
	cp libasmrund.a $(LIBDIR)/libasmrund.a
	cd $(LIBDIR); $(RANLIB) libasmrund.a
.PHONY: install-runtimed

install-noprof:
	rm -f $(LIBDIR)/libasmrunp.a; ln -s libasmrun.a $(LIBDIR)/libasmrunp.a

install-prof:
	cp libasmrunp.a $(LIBDIR)/libasmrunp.a
	cd $(LIBDIR); $(RANLIB) libasmrunp.a

power-bsd_elf.S: power-elf.S
	cp power-elf.S power-bsd_elf.S

power.o: power-$(SYSTEM).o
	cp power-$(SYSTEM).o power.o

power.p.o: power-$(SYSTEM).o
	cp power-$(SYSTEM).o power.p.o

main.c: ../byterun/main.c
	ln -s ../byterun/main.c main.c
misc.c: ../byterun/misc.c
	ln -s ../byterun/misc.c misc.c
freelist.c: ../byterun/freelist.c
	ln -s ../byterun/freelist.c freelist.c
major_gc.c: ../byterun/major_gc.c
	ln -s ../byterun/major_gc.c major_gc.c
minor_gc.c: ../byterun/minor_gc.c
	ln -s ../byterun/minor_gc.c minor_gc.c
memory.c: ../byterun/memory.c
	ln -s ../byterun/memory.c memory.c
alloc.c: ../byterun/alloc.c
	ln -s ../byterun/alloc.c alloc.c
array.c: ../byterun/array.c
	ln -s ../byterun/array.c array.c
compare.c: ../byterun/compare.c
	ln -s ../byterun/compare.c compare.c
ints.c: ../byterun/ints.c
	ln -s ../byterun/ints.c ints.c
floats.c: ../byterun/floats.c
	ln -s ../byterun/floats.c floats.c
str.c: ../byterun/str.c
	ln -s ../byterun/str.c str.c
io.c: ../byterun/io.c
	ln -s ../byterun/io.c io.c
extern.c: ../byterun/extern.c
	ln -s ../byterun/extern.c extern.c
intern.c: ../byterun/intern.c
	ln -s ../byterun/intern.c intern.c
hash.c: ../byterun/hash.c
	ln -s ../byterun/hash.c hash.c
sys.c: ../byterun/sys.c
	ln -s ../byterun/sys.c sys.c
parsing.c: ../byterun/parsing.c
	ln -s ../byterun/parsing.c parsing.c
gc_ctrl.c: ../byterun/gc_ctrl.c
	ln -s ../byterun/gc_ctrl.c gc_ctrl.c
terminfo.c: ../byterun/terminfo.c
	ln -s ../byterun/terminfo.c terminfo.c
md5.c: ../byterun/md5.c
	ln -s ../byterun/md5.c md5.c
obj.c: ../byterun/obj.c
	ln -s ../byterun/obj.c obj.c
lexing.c: ../byterun/lexing.c
	ln -s ../byterun/lexing.c lexing.c
printexc.c: ../byterun/printexc.c
	ln -s ../byterun/printexc.c printexc.c
callback.c: ../byterun/callback.c
	ln -s ../byterun/callback.c callback.c
weak.c: ../byterun/weak.c
	ln -s ../byterun/weak.c weak.c
compact.c: ../byterun/compact.c
	ln -s ../byterun/compact.c compact.c
finalise.c: ../byterun/finalise.c
	ln -s ../byterun/finalise.c finalise.c
custom.c: ../byterun/custom.c
	ln -s ../byterun/custom.c custom.c
meta.c: ../byterun/meta.c
	ln -s ../byterun/meta.c meta.c
globroots.c: ../byterun/globroots.c
	ln -s ../byterun/globroots.c globroots.c
unix.c: ../byterun/unix.c
	ln -s ../byterun/unix.c unix.c
dynlink.c: ../byterun/dynlink.c
	ln -s ../byterun/dynlink.c dynlink.c
signals.c: ../byterun/signals.c
	ln -s ../byterun/signals.c signals.c
debugger.c: ../byterun/debugger.c
	ln -s ../byterun/debugger.c debugger.c

LINKEDFILES=misc.c freelist.c major_gc.c minor_gc.c memory.c alloc.c array.c \
  compare.c ints.c floats.c str.c io.c extern.c intern.c hash.c sys.c \
  parsing.c gc_ctrl.c terminfo.c md5.c obj.c lexing.c printexc.c callback.c \
  weak.c compact.c finalise.c meta.c custom.c main.c globroots.c unix.c \
  dynlink.c signals.c debugger.c

clean::
	rm -f $(LINKEDFILES)

.SUFFIXES: .S .d.o .p.o

.S.o:
	$(ASPP) -DSYS_$(SYSTEM) -DMODEL_$(MODEL) -o $*.o $*.S || \
	{ echo "If your assembler produced syntax errors, it is probably";\
          echo "unhappy with the preprocessor. Check your assembler, or";\
          echo "try producing $*.o by hand.";\
          exit 2; }

.S.p.o:
	$(ASPP) -DSYS_$(SYSTEM) $(ASPPPROFFLAGS) -o $*.p.o $*.S

.c.d.o:
	ln -s -f $*.c $*.d.c
	$(CC) -c $(DFLAGS) $*.d.c
	rm -f $*.d.c

.c.p.o:
	ln -s -f $*.c $*.p.c
	$(CC) -c $(PFLAGS) $*.p.c
	rm -f $*.p.c

.s.o:
	$(ASPP) -DSYS_$(SYSTEM) -o $*.o $*.s

.s.p.o:
	$(ASPP) -DSYS_$(SYSTEM) $(ASPPPROFFLAGS) -o $*.p.o $*.s

clean::
	rm -f *.o *.a *~

depend: $(COBJS:.o=.c) ${LINKEDFILES}
	-gcc -MM $(FLAGS) *.c > .depend
	gcc -MM $(FLAGS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' >> .depend
	gcc -MM $(FLAGS) -DDEBUG *.c | sed -e 's/\.o/.p.o/' >> .depend

include .depend
