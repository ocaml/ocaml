# Makefile for the parser generator.

include ../config/Makefile

CC=$(BYTECC)
CFLAGS=-O -DNDEBUG $(BYTECCCOMPOPTS)

OBJS= closure.o error.o lalr.o lr0.o main.o mkpar.o output.o reader.o \
  skeleton.o symtab.o verbose.o warshall.o

all: cslyacc

cslyacc: $(OBJS)
	$(CC) $(CFLAGS) $(CCLINKFLAGS) -o cslyacc $(OBJS)

clean:
	rm -f *.o cslyacc *~

depend:

closure.o: defs.h
error.o: defs.h
lalr.o: defs.h
lr0.o: defs.h
main.o: defs.h
mkpar.o: defs.h
output.o: defs.h
reader.o: defs.h
skeleton.o: defs.h
symtab.o: defs.h
verbose.o: defs.h
warshall.o: defs.h
