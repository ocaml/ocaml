include ../config/Makefile.h
include ../Makefile.config

CFLAGS=-I../byterun -DTARGET_$(ARCH) -O $(CCCOMPOPTS)
DFLAGS=-I../../byterun -DTARGET_$(ARCH) -g -DDEBUG $(CCCOMPOPTS)

COBJS=fail.o main.o roots.o signals.o
ASMOBJS=$(ARCH).o
OTHEROBJS=../byterun/misc.o ../byterun/freelist.o \
  ../byterun/major_gc.o ../byterun/minor_gc.o ../byterun/memory.o \
  ../byterun/alloc.o ../byterun/compare.o \
  ../byterun/ints.o ../byterun/floats.o ../byterun/str.o \
  ../byterun/array.o ../byterun/io.o ../byterun/extern.o \
  ../byterun/intern.o ../byterun/hash.o ../byterun/sys.o \
  ../byterun/parsing.o ../byterun/gc_ctrl.o \
  ../byterun/terminfo.o ../byterun/crc.o ../byterun/obj.o

OBJS=$(COBJS) $(ASMOBJS) $(OTHEROBJS)
DOBJS=$(COBJS:.o=.d.o) $(ASMOBJS) $(OTHEROBJS:.o=.d.o)

all: libasmrun.a libasmrund.a

libasmrun.a: $(OBJS)
	rm -f libasmrun.a
	ar rc libasmrun.a $(OBJS)
	$(RANLIB) libasmrun.a

libasmrund.a: $(DOBJS)
	rm -f libasmrund.a
	ar rc libasmrund.a $(DOBJS)
	$(RANLIB) libasmrund.a

.SUFFIXES: .asm .d.o

.asm.o:
	$(AS) $(ASFLAGS) -o $*.o $*.asm

.c.d.o:
	cd .debugobj; $(CC) -c $(DFLAGS) -I.. ../$<
	mv .debugobj/$*.o $*.d.o

clean::
	rm -f *.o *.s *.a *~

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	gcc -MM $(DFLAGS) *.c | sed -e 's/\.o/.d.o/' >> .depend

include .depend

