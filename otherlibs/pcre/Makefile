#########################################################################
#                                                                       #
#                         PCRE direct support                           #
#                                                                       #
#                      Jun FURUSE, Univ. of Tokyo                       #
#                                                                       #
#   Copyright 2005 Jun FURUSE. All rights reserved.                     #
#   This file is distributed under the terms of the GNU Library General #
#   Public License, with the special exception on linking described in  #
#   file ../../LICENSE.                                                 #
#                                                                       #
#########################################################################

# $Id$

# Makefile for the str library

include ../../config/Makefile

# Compilation options
CC=$(BYTECC)
CFLAGS=-O -I../../byterun $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)
CAMLC=../../ocamlcomp.sh
CAMLOPT=../../ocamlcompopt.sh
COMPFLAGS=-warn-error Agy
COBJS=pcre_stubs.o
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib
PCRE_LINK=-lpcre

all: libpcre.a pcre.cmi pcre.cma

allopt: libpcre.a pcre.cmi pcre.cmxa

libpcre.a: $(COBJS)
	$(MKLIB) -oc pcre $(COBJS) $(PCRE_LINK)

pcre.cma: pcre.cmo pcrex.cmo
	$(MKLIB) -ocamlc '$(CAMLC)' -o pcre -oc pcre pcre.cmo pcrex.cmo $(PCRE_LINK)


pcre.cmxa: pcre.cmx pcrex.cmx
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o pcre -oc pcre pcre.cmx pcrex.cmx $(PCRE_LINK)

pcre.cmx: ../../ocamlopt

partialclean:
	rm -f *.cm*

clean:: partialclean
	rm -f *.a *.so *.o

install:
	if test -f dllpcre.so; then cp dllpcre.so $(STUBLIBDIR)/dllpcre.so; fi
	cp libpcre.a $(LIBDIR)/libpcre.a
	cd $(LIBDIR); $(RANLIB) libpcre.a
	cp pcre.cma *.cmi *.mli $(LIBDIR)

installopt:
	cp *.cmx pcre.cmxa pcre.a $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) pcre_stubs.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend: 
	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
