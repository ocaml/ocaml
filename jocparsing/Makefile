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
INCLUDES= -I ../stdlib -I ../camlp4/camlp4 -I ../camlp4/meta -I ../camlp4/lib
RUNTIME=../boot/ocamlrun
COMPILER=../ocamlc $(INCLUDES)
CAMLC=$(RUNTIME) $(COMPILER)
PARSER=../camlp4/etc/camlp4o $(INCLUDES) pa_extend.cmo
CAMLP4O=$(RUNTIME) $(PARSER)
pa_joc.cmo: pa_joc.ml
	$(CAMLC) -dlambda -pp "$(CAMLP4O)" -c pa_joc.ml

test:
	$(CAMLC) -dparsetree -pp "$(RUNTIME) ../camlp4/etc/camlp4o ./pa_joc.cmo" t.ml