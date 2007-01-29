#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../../LICENSE.   #
#                                                                       #
#########################################################################

# $Id$

# Makefile for the portable graphics library

include ../../config/Makefile

CC=$(BYTECC)
CFLAGS=-I../../byterun $(X11_INCLUDES) -O $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)
CAMLC=../../ocamlcomp.sh
CAMLOPT=../../ocamlcompopt.sh
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
COMPFLAGS=-warn-error A -g

OBJS=open.o draw.o fill.o color.o text.o \
  image.o make_img.o dump_img.o point_col.o sound.o events.o \
  subwindow.o

CAMLOBJS=graphics.cmo graphicsX11.cmo

all: libgraphics.a graphics.cmi graphics.cma

allopt: libgraphics.a graphics.cmi graphics.cmxa

libgraphics.a: $(OBJS)
	$(MKLIB) -o graphics $(OBJS) $(X11_LINK)

graphics.cma: $(CAMLOBJS)
	$(MKLIB) -ocamlc '$(CAMLC)' -o graphics $(CAMLOBJS) $(X11_LINK)

graphics.cmxa: $(CAMLOBJS:.cmo=.cmx)
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o graphics $(CAMLOBJS:.cmo=.cmx) $(X11_LINK)

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.so *.o

install:
	if test -f dllgraphics.so; then cp dllgraphics.so $(STUBLIBDIR)/dllgraphics.so; fi
	cp libgraphics.a $(LIBDIR)/libgraphics.a
	cd $(LIBDIR); $(RANLIB) libgraphics.a
	cp graphics.cm[ia] graphicsX11.cmi graphics.mli graphicsX11.mli $(LIBDIR)

installopt:
	cp graphics.cmx graphics.cmxa graphics.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) graphics.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<
.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<
.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
	gcc -MM $(CFLAGS) *.c | sed -e 's, /usr[^ ]*\.h,,g' > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
