# Makefile for the portable graphics library

include ../../Makefile.config

CC=$(BYTECC)
CFLAGS=-I../../byterun -I$(X11_INCLUDES) -O $(BYTECCCOMPOPTS)

CAMLC=../../boot/cslrun ../../boot/cslc -I ../../boot
CAMLOPT=../../boot/cslrun ../../cslopt -I ../../stdlib

OBJS=open.o draw.o fill.o color.o text.o \
  image.o make_img.o dump_img.o point_col.o sound.o events.o

all: libgraph.a graphics.cmi graphics.cma

allopt: libgraph.a graphics.cmi graphics.cmxa

libgraph.a: $(OBJS)
	rm -f libgraph.a
	ar rc libgraph.a $(OBJS)
	$(RANLIB) libgraph.a

graphics.cma: graphics.cmo
	$(CAMLC) -a -o graphics.cma graphics.cmo

graphics.cmxa: graphics.cmx
	$(CAMLOPT) -a -o graphics.cmxa graphics.cmx

clean:
	rm -f *.cm*

realclean: clean
	rm -f *.a *.o $(GENFILES)

install:
	cp libgraph.a $(LIBDIR)/libgraph.a
	cd $(LIBDIR); $(RANLIB) libgraph.a
	cp graphics.cm[ia] $(LIBDIR)

installopt:
	cp graphics.a $(LIBDIR)/graphics.a
	cd $(LIBDIR); $(RANLIB) graphics.a
	cp graphics.cmxa $(LIBDIR)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmo:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../tools/csldep *.mli *.ml >> .depend

include .depend
