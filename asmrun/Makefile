include ../config/Makefile.h
include ../Makefile.config

VPATH=.:../byterun

CFLAGS=-I../byterun -DTARGET_$(ARCH) -O $(CCCOMPOPTS)
DFLAGS=-I../../byterun -DTARGET_$(ARCH) -g -DDEBUG $(CCCOMPOPTS)

COBJS=misc.o main.o fail.o signals.o \
  freelist.o major_gc.o minor_gc.o memory.o alloc.o roots.o \
  compare.o ints.o floats.o str.o array.o io.o extern.o intern.o \
  hash.o sys.o parsing.o gc_ctrl.o terminfo.o crc.o obj.o
ASMOBJS=$(ARCH).o

OBJS=$(COBJS) $(ASMOBJS)
DOBJS=$(COBJS:.o=.d.o) $(ASMOBJS)

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
	gcc -MM $(CFLAGS) *.c ../byterun/*.c > .depend
	gcc -MM $(CFLAGS) -DDEBUG *.c ../byterun/*.c | sed -e 's/\.o/.d.o/' >> .depend

include .depend

