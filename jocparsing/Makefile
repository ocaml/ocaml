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

include ../config/Makefile

INCLUDES= -I ../stdlib -I ../camlp4/camlp4 -I ../camlp4/etc -I ../camlp4/odyl -I ../camlp4/meta -I ../camlp4/lib
RUNTIME=../boot/ocamlrun
COMPILER=../ocamlc $(INCLUDES)
CAMLC=$(RUNTIME) $(COMPILER)
PARSER=../camlp4/etc/camlp4o $(INCLUDES) pa_extend.cmo
CAMLP4O=$(RUNTIME) $(PARSER)

all: jocp

pa_joc.cmo: pa_joc.ml
	$(CAMLC) $(INCLUDES) -pp "$(CAMLP4O)" -c pa_joc.ml

jocp: pa_joc.cmo
	$(CAMLC) $(INCLUDES) -o jocp odyl.cma camlp4.cma pa_o.cmo pa_op.cmo pr_dump.cmo pa_joc.cmo odyl.cmo -linkall

install:
	cp jocp $(BINDIR)

clean:
	/bin/rm -f *~ pa_joc.cmo jocp

depend:
	$(RUNTIME) ../../tools/ocamldep *.mli *.ml >> .depend

include .depend