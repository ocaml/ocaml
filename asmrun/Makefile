include ../config/Makefile

CC=$(NATIVECC)
FLAGS=-I../byterun -DNATIVE_CODE -DTARGET_$(ARCH) -DSYS_$(SYSTEM) 
CFLAGS=$(FLAGS) -O $(NATIVECCCOMPOPTS)
DFLAGS=$(FLAGS) -g -DDEBUG $(NATIVECCCOMPOPTS)
PFLAGS=$(FLAGS) -pg -DPROFILING $(NATIVECCCOMPOPTS)

COBJS=startup.o main.o fail.o roots.o signals.o \
  misc.o freelist.o major_gc.o minor_gc.o memory.o alloc.o compare.o ints.o \
  floats.o str.o array.o io.o extern.o intern.o hash.o sys.o parsing.o \
  gc_ctrl.o terminfo.o md5.o obj.o lexing.o printexc.o callback.o weak.o \
  compact.o

ASMOBJS=$(ARCH).o

OBJS=$(COBJS) $(ASMOBJS)
DOBJS=$(COBJS:.o=.d.o) $(ASMOBJS)
POBJS=$(COBJS:.o=.p.o) $(ASMOBJS:.o=.p.o)

all: libasmrun.a libasmrunp.a

libasmrun.a: $(OBJS)
	rm -f libasmrun.a
	ar rc libasmrun.a $(OBJS)
	$(RANLIB) libasmrun.a

libasmrund.a: $(DOBJS)
	rm -f libasmrund.a
	ar rc libasmrund.a $(DOBJS)
	$(RANLIB) libasmrund.a

libasmrunp.a: $(POBJS)
	rm -f libasmrunp.a
	ar rc libasmrunp.a $(POBJS)
	$(RANLIB) libasmrunp.a

install:
	cp libasmrun.a libasmrunp.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) libasmrun.a libasmrunp.a

power.o: power-$(SYSTEM).o
	cp power-$(SYSTEM).o power.o

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
meta.c: ../byterun/meta.c
	ln -s ../byterun/meta.c meta.c

LINKEDFILES=misc.c freelist.c major_gc.c minor_gc.c memory.c alloc.c array.c \
  compare.c ints.c floats.c str.c io.c extern.c intern.c hash.c sys.c \
  parsing.c gc_ctrl.c terminfo.c md5.c obj.c lexing.c printexc.c callback.c \
  weak.c compact.c meta.c

clean::
	rm -f $(LINKEDFILES)

.SUFFIXES: .S .d.o .p.o

.S.o:
	$(ASPP) $(ASPPFLAGS) -o $*.o $*.S

.S.p.o:
	$(ASPP) $(ASPPFLAGS) -DPROFILING -o $*.p.o $*.S

.c.d.o:
	@ if test -f $*.o; then mv $*.o $*.f.o; else :; fi
	$(CC) -c $(DFLAGS) $<
	mv $*.o $*.d.o
	@ if test -f $*.f.o; then mv $*.f.o $*.o; else :; fi

.c.p.o:
	@ if test -f $*.o; then mv $*.o $*.f.o; else :; fi
	$(CC) -c $(PFLAGS) $<
	mv $*.o $*.p.o
	@ if test -f $*.f.o; then mv $*.f.o $*.o; else :; fi

clean::
	rm -f *.o *.s *.a *~

depend: $(COBJS:.o=.c)
	gcc -MM $(FLAGS) *.c > .depend
	gcc -MM $(FLAGS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' >> .depend

include .depend

