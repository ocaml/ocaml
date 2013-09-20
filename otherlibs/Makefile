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

# Common Makefile for otherlibs on the Unix ports

CAMLC=$(CAMLC_BIN) -nostdlib -I ../../stdlib
CAMLOPT=$(CAMLOPT_BIN) -nostdlib -I ../../stdlib
CAMLC_BIN_CMD=$(CAMLC_BIN_CMD_TO_EVAL) -nostdlib -I ../../stdlib
CAMLOPT_BIN_CMD=$(CAMLOPT_BIN_CMD_TO_EVAL) -nostdlib -I ../../stdlib
CFLAGS=-I../../byterun -O $(SHAREDCCCOMPOPTS) $(EXTRACFLAGS)

include ../Makefile.shared
# Note .. is the current directory (this makefile is included from
# a subdirectory)
