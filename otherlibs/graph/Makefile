# Makefile for the portable graphics library

include ../../config/Makefile

CC=$(BYTECC)
CFLAGS=-I../../byterun -I$(X11_INCLUDES) -O $(BYTECCCOMPOPTS)

CAMLC=../../boot/ocamlrun ../../boot/ocamlc -I ../../boot
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib

OBJS=open.o draw.o fill.o color.o text.o \
  image.o make_img.o dump_img.o point_col.o sound.o events.o

all: libgraphics.a graphics.cmi graphics.cma

allopt:

libgraphics.a: $(OBJS)
	rm -f libgraphics.a
	ar rc libgraphics.a $(OBJS)
	$(RANLIB) libgraphics.a

graphics.cma: graphics.cmo
	$(CAMLC) -a -o graphics.cma graphics.cmo

graphics.ml: graphics.mlp
	$(CPP) -DUSE_ASYNC_IO graphics.mlp > graphics.ml

clean:
	rm -f *.cm*
	rm -f graphics.ml

realclean: clean
	rm -f *.a *.o

install:
	cp libgraphics.a $(LIBDIR)/libgraphics.a
	cd $(LIBDIR); $(RANLIB) libgraphics.a
	cp graphics.cm[ia] $(LIBDIR)

installopt:

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmo:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend: graphics.ml
	gcc -MM $(CFLAGS) *.c > .depend
	../../tools/ocamldep *.mli *.ml >> .depend

include .depend
