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

compile: compile-targets

promote: defaultpromote

clean: defaultclean

include ../makefiles/Makefile.common

compile-targets: testing.cmi testing.cmo
	@if [ -z "$(BYTECODE_ONLY)" ]; then \
	  $(MAKE) testing.cmx; \
	fi
