ARCH=alpha
CC=gcc
CFLAGS=-O2 -Wall
#CFLAGS=-g -DDEBUG -Wall
AS=as
ASFLAGS=-O2 -g

OBJS=runtime.o gc.o debug.o compare.o $(ARCH).o

librun.a: $(OBJS)
	rm -f librun.a
	ar rc librun.a $(OBJS)
	ranlib librun.a

.SUFFIXES: .asm .o

.asm.o:
	$(AS) $(ASFLAGS) -o $*.o $*.asm

clean::
	rm -f *.o *.s *.a *~

runtime.o gc.o compare.o debug.o: mlvalues.h misc.h
