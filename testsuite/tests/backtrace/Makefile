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
EXECNAME=./program

run-all:
	@for file in *.ml; do \
	  $(OCAMLC) -g -o $(EXECNAME) $$file; \
	  for arg in a b c d ''; do \
	    printf " ... testing '$$file' (with argument '$$arg'):"; \
	    (OCAMLRUNPARAM=b=1 $(EXECNAME) $$arg || true) > `basename $$file ml`$$arg.result 2>&1; \
	    $(DIFF) `basename $$file ml`$$arg.reference `basename $$file ml`$$arg.result > /dev/null && echo " => passed" || (echo " => failed" && exit 1); \
	  done; \
	done

promote: defaultpromote

clean: defaultclean
	@rm -f *.result $(EXECNAME)

include $(BASEDIR)/makefiles/Makefile.common
