#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License.          #
#                                                                       #
#########################################################################

# $Id$

# Makefile for the portable graphics library

include ../../config/Makefile

CC=$(BYTECC)
CFLAGS=-I../../byterun $(X11_INCLUDES) -O $(BYTECCCOMPOPTS)

CAMLC=../../boot/ocamlrun ../../ocamlc -I ../../stdlib
CAMLOPT=../../boot/ocamlrun ../../ocamlopt -I ../../stdlib

OBJS=open.o draw.o fill.o color.o text.o \
  image.o make_img.o dump_img.o point_col.o sound.o events.o \
  subwindow.o

all: libgraphics.a graphics.cmi graphics.cma

allopt: libgraphics.a graphics.cmi graphics.cmxa

libgraphics.a: $(OBJS)
	rm -f libgraphics.a
	ar rc libgraphics.a $(OBJS)
	$(RANLIB) libgraphics.a

graphics.cma: graphics.cmo
	$(CAMLC) -a -o graphics.cma -custom \
          graphics.cmo -cclib -lgraphics $(X11_LINK)

graphics.cmxa: graphics.cmx
	$(CAMLOPT) -a -o graphics.cmxa graphics.cmx -cclib -lgraphics $(X11_LINK)

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.o

install:
	cp libgraphics.a $(LIBDIR)/libgraphics.a
	cd $(LIBDIR); $(RANLIB) libgraphics.a
	cp graphics.cm[ia] graphics.mli $(LIBDIR)

installopt:
	cp graphics.cmxa graphics.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) graphics.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmo:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
