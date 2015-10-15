#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#                 Xavier Clerc, SED, INRIA Rocquencourt                 #
#                                                                       #
#   Copyright 2010 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

BASEDIR=../..

INCLUDES=\
  -I $(OTOPDIR)/parsing \
  -I $(OTOPDIR)/utils \
  -I $(OTOPDIR)/typing \
  -I $(OTOPDIR)/bytecomp \
  -I $(OTOPDIR)/asmcomp

OTHEROBJS=\
  $(OTOPDIR)/compilerlibs/ocamlcommon.cma \
  $(OTOPDIR)/compilerlibs/ocamloptcomp.cma

OBJS= \
	test_utils.cmo \
	test_flambdacheck.cmo \
	test_flambdagen.cmo \
	test_connected_components.cmo \
	test_kept_parameters.cmo \
	test_recursives.cmo
	# test_movelets.cmo test_flambdaifstaticraise.cmo \
	# test_purity.cmo test_flambdareach.cmo

ADD_COMPFLAGS=$(INCLUDES) -g

all: run

main.cmo: $(OBJS:.cmo=.cmi)

main: $(OBJS:.cmo=.cmi) $(OBJS) main.cmo
	@$(OCAMLC) $(LINKFLAGS) -g -o main $(OTHEROBJS) $(OBJS) main.cmo

run: main
	@printf " ... testing 'flambda-unit-tests':"; \
	$(OCAMLRUN) main > unit_test.result \
        && $(DIFF) unit_test.result unit_test.reference \
	&& echo " => passed" || echo " => failed"

clean: defaultclean
	@rm -f ./main *.out

include $(BASEDIR)/makefiles/Makefile.common

promote:
