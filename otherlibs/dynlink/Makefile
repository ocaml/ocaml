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

# Makefile for the dynamic link library

include ../../config/Makefile

CAMLC=../../boot/ocamlrun ../../ocamlc
INCLUDES=-I ../../utils -I ../../typing -I ../../bytecomp
COMPFLAGS=-warn-error A -I ../../stdlib $(INCLUDES)

OBJS=dynlink.cmo
COMPILEROBJS=misc.cmo config.cmo tbl.cmo clflags.cmo consistbl.cmo \
  ident.cmo path.cmo \
  types.cmo btype.cmo predef.cmo runtimedef.cmo \
  bytesections.cmo dll.cmo meta.cmo symtable.cmo opcodes.cmo

all: dynlink.cma extract_crc

allopt:

dynlink.cma: $(OBJS)
	$(CAMLC) $(COMPFLAGS) -a -o dynlink.cma $(COMPILEROBJS) $(OBJS)

extract_crc: dynlink.cma extract_crc.cmo
	$(CAMLC) $(COMPFLAGS) -o extract_crc dynlink.cma extract_crc.cmo

install:
	cp dynlink.cmi dynlink.cma dynlink.mli extract_crc $(LIBDIR)

installopt:

partialclean:
	rm -f extract_crc *.cm[ioa]

clean: partialclean

.SUFFIXES: .ml .mli .cmo .cmi

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

depend:
	../../boot/ocamlrun ../../tools/ocamldep $(INCLUDES) *.mli *.ml >.depend

include .depend
