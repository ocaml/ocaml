include ../config/Makefile.h
include ../Makefile.config

CC=$(NATIVECC)
CFLAGS=-I../byterun -DTARGET_$(ARCH) -O $(NATIVECCCOMPOPTS)
DFLAGS=-I../byterun -DTARGET_$(ARCH) -g -DDEBUG $(NATIVECCCOMPOPTS)

VPATH=.:../byterun

COBJS=main.o fail.o roots.o signals.o \
  misc.o freelist.o major_gc.o minor_gc.o memory.o alloc.o compare.o ints.o \
  floats.o str.o array.o io.o extern.o intern.o hash.o sys.o parsing.o \
  gc_ctrl.o terminfo.o crc.o obj.o
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
	@ if test -f $*.o; then mv $*.o $*.f.o; else :; fi
	$(CC) -c $(DFLAGS) $<
	mv $*.o $*.d.o
	@ if test -f $*.f.o; then mv $*.f.o $*.o; else :; fi

clean::
	rm -f *.o *.s *.a *~

depend:
	gcc -MM $(CFLAGS) *.c ../byterun/*.c > .depend
	gcc -MM $(CFLAGS) -DDEBUG *.c ../byterun/*.c | sed -e 's/\.o/.d.o/' >> .depend

include .depend

