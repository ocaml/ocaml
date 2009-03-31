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

# Makefile for the ndbm library

LIBNAME=dbm
CLIBNAME=mldbm
CAMLOBJS=dbm.cmo
COBJS=cldbm.o
EXTRACFLAGS=$(DBM_INCLUDES)
LINKOPTS=$(DBM_LINK)
LDOPTS=-ldopt "$(DBM_LINK)"

include ../Makefile


depend:
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml > .depend

include .depend
