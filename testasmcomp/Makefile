ARCH=alpha

CODEGEN=../codegen
ASFLAGS=-O2
CFLAGS=-g

PROGS=fib tak quicksort quicksort2 soli integr

all: $(PROGS)

fib: main.c fib.o $(ARCH).o
	$(CC) $(CFLAGS) -o fib -DINT_INT -DFUN=fib main.c fib.o $(ARCH).o

tak: main.c tak.o $(ARCH).o
	$(CC) $(CFLAGS) -o tak -DUNIT_INT -DFUN=takmain main.c tak.o $(ARCH).o

quicksort: main.c quicksort.o $(ARCH).o
	$(CC) $(CFLAGS) -o quicksort -DSORT -DFUN=quicksort main.c quicksort.o $(ARCH).o

quicksort2: main.c quicksort2.o $(ARCH).o
	$(CC) $(CFLAGS) -o quicksort2 -DSORT -DFUN=quicksort main.c quicksort2.o $(ARCH).o

soli: main.c soli.o $(ARCH).o
	$(CC) $(CFLAGS) -o soli -DUNIT_INT -DFUN=solitaire main.c soli.o $(ARCH).o

integr: main.c integr.o $(ARCH).o
	$(CC) $(CFLAGS) -o integr -DINT_FLOAT -DFUN=test main.c integr.o $(ARCH).o

tagged-fib: main.c tagged-fib.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-fib -DINT_INT -DFUN=fib main.c tagged-fib.o $(ARCH).o

tagged-tak: main.c tagged-tak.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-tak -DUNIT_INT -DFUN=takmain main.c tagged-tak.o $(ARCH).o

tagged-quicksort: main.c tagged-quicksort.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-quicksort -DSORT -DFUN=quicksort main.c tagged-quicksort.o $(ARCH).o

tagged-integr: main.c tagged-integr.o $(ARCH).o
	$(CC) $(CFLAGS) -o tagged-integr -DINT_FLOAT -DFUN=test main.c tagged-integr.o $(ARCH).o

.SUFFIXES:
.SUFFIXES: .cmm .c .o .asm

.cmm.o:
	cslrun $(CODEGEN) $*.cmm > $*.s
	$(AS) $(ASFLAGS) -o $*.o $*.s

.asm.o:
	$(AS) $(ASFLAGS) -o $*.o $*.asm

clean:
	rm -f *.s *.o *~ $(PROGS)

fib.o tak.o quicksort.o quicksort2.o integr.o soli.o: $(CODEGEN)
