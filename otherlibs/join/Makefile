#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Luc Maranget, projet Moscova, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2001 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License.          #
#                                                                       #
#########################################################################
# $Id$

include ../../config/Makefile

INCLUDES=-I ../../stdlib
RUNTIME=../../boot/ocamlrun
COMPILER=../../ocamlc $(INCLUDES)
CAMLC=$(RUNTIME) $(COMPILER) $(INCLUDES)

all: jprims.cmi

jprims.cmi: jprims.mli
	$(CAMLC) -c jprims.mli

clean:
	/bin/rm -f *~ *.cmi

install:
	cp jprims.cmi $(LIBDIR)

depend:
#	gcc -MM $(CFLAGS) *.c > .depend
	$(RUNTIME) ../../tools/ocamldep *.mli *.ml >> .depend

include .depend