ARCH=alpha
CC=gcc
CFLAGS=-g
AS=as
ASFLAGS=-O2 -g

OBJS=runtime.o $(ARCH).o

librun.a: $(OBJS)
	rm -f librun.a
	ar rc librun.a $(OBJS)
	ranlib librun.a

.SUFFIXES: .asm .o

.asm.o:
	$(AS) $(ASFLAGS) -o $*.o $*.asm

clean::
	rm -f *.o *.s *.a *~
