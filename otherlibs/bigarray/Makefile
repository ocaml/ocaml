#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../../LICENSE.   #
#                                                                       #
#########################################################################

LIBNAME=bigarray
EXTRACFLAGS=-I../$(UNIXLIB) -DIN_OCAML_BIGARRAY -DCAML_NAME_SPACE
EXTRACAMLFLAGS=-I ../$(UNIXLIB)
mmap_impl=mmap_$(UNIX_OR_WIN32)
COBJS=bigarray_stubs.$(O) $(mmap_impl).$(O)
CAMLOBJS=bigarray.cmo
HEADERS=bigarray.h

include ../Makefile

depend:
	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
