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
INCLUDES=
RUNTIME=../../boot/ocamlrun
COMPILER=../../ocamlc $(INCLUDES)
CAMLC=$(RUNTIME) $(COMPILER)

jprims.cmi: jprims.mli
	$(CAMLC) -c jprims.mli