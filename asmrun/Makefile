include ../config/Makefile.h
include ../Makefile.config

CC=$(NATIVECC)
CFLAGS=-I../byterun -DTARGET_$(ARCH) -DSYS_$(SYSTEM) -O $(NATIVECCCOMPOPTS)
DFLAGS=-I../byterun -DTARGET_$(ARCH) -DSYS_$(SYSTEM) -g -DDEBUG $(NATIVECCCOMPOPTS)

COBJS=main.o fail.o roots.o signals.o \
  misc.o freelist.o major_gc.o minor_gc.o memory.o alloc.o compare.o ints.o \
  floats.o str.o array.o io.o extern.o intern.o hash.o sys.o parsing.o \
  gc_ctrl.o terminfo.o crc.o obj.o
ASMOBJS=$(ARCH).o

OBJS=$(COBJS) $(ASMOBJS)
DOBJS=$(COBJS:.o=.d.o) $(ASMOBJS)

all: libasmrun.a

libasmrun.a: $(OBJS)
	rm -f libasmrun.a
	ar rc libasmrun.a $(OBJS)
	$(RANLIB) libasmrun.a

libasmrund.a: $(DOBJS)
	rm -f libasmrund.a
	ar rc libasmrund.a $(DOBJS)
	$(RANLIB) libasmrund.a

install:
	cp libasmrun.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) libasmrun.a

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
crc.c: ../byterun/crc.c
	ln -s ../byterun/crc.c crc.c
obj.c: ../byterun/obj.c
	ln -s ../byterun/obj.c obj.c

.SUFFIXES: .asm .d.o

.asm.o:
	$(AS) $(ASFLAGS) -o $*.o $*.asm

.c.d.o:
	@ if test -f $*.o; then mv $*.o $*.f.o; else :; fi
	$(CC) -c $(DFLAGS) $<
	mv $*.o $*.d.o
	@ if test -f $*.f.o; then mv $*.f.o $*.o; else :; fi

clean::
	rm -f *.o *.s *.a *~

depend:
	gcc -MM *.c ../byterun/*.c > .depend
	gcc -MM -DDEBUG *.c ../byterun/*.c | sed -e 's/\.o/.d.o/' >> .depend

include .depend

