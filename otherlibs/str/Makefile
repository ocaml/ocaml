# Makefile for the str library

include ../../Makefile.config

# Compilation options
CC=$(BYTECC)
CFLAGS=-O -I$(REGEXLIB) -I../../byterun $(BYTECCCOMPOPTS)
CAMLC=../../boot/camlrun ../../boot/camlc -I ../../boot
REGEXLIB=regex-0.12
COBJS=strstubs.o $(REGEXLIB)/regex.o

all: libstr.a str.cma

libstr.a: $(COBJS)
	rm -f libstr.a
	ar rc libstr.a $(COBJS)
	$(RANLIB) libstr.a

str.cma: str.cmo
	$(CAMLC) -a -o str.cma str.cmo

$(REGEXLIB)/regex.o: $(REGEXLIB)/regex.c $(REGEXLIB)/regex.h
	cd $(REGEXLIB); CC="$(CC)" sh configure; make

clean:
	rm -f libstr.a *.o *.cm[ioa] camlstr
	rm -f *.dvi *.log *.aux
	cd $(REGEXLIB); if test -f Makefile; then make distclean; else exit 0; fi

install:
	cp libstr.a $(LIBDIR)/libstr.a
	cd $(LIBDIR); $(RANLIB) libstr.a
	cp str.cma str.cmi $(LIBDIR)

.SUFFIXES: .ml .mli .cmo .cmi

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../tools/camldep *.mli *.ml >> .depend

include .depend
