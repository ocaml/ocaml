# Makefile for the portable graphics library

include ../../Makefile.config

CC=$(BYTECC)
CFLAGS=-I../../byterun -O $(BYTECCCOMPOPTS)

CAMLC=../../boot/camlrun ../../boot/camlc -I ../../boot

OBJS=open.o draw.o fill.o color.o text.o \
  image.o make_img.o dump_img.o point_col.o sound.o events.o

all: libgraph.a graphics.cmi graphics.cma

libgraph.a: $(OBJS)
	rm -f libgraph.a
	ar rc libgraph.a $(OBJS)
	$(RANLIB) libgraph.a

graphics.cma: graphics.cmo
	$(CAMLC) -a -o graphics.cma graphics.cmo

clean::
	rm -f libgraph.a $(GENFILES) *.o *.cm[ioa]

install:
	cp libgraph.a $(LIBDIR)/libgraph.a
	cd $(LIBDIR); $(RANLIB) libgraph.a
	cp graphics.cm[ia] $(LIBDIR)

.SUFFIXES: .ml .mli .cmo .cmi

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmo:
	$(CAMLC) -c $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../tools/camldep *.mli *.ml >> .depend

include .depend
