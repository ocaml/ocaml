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
EXECNAME=./program$(EXE)

.PHONY: run-all
run-all:
	@for file in *.ml; do \
	  $(OCAMLC) -g -o $(EXECNAME) $$file; \
	  for arg in a b c d ''; do \
	    printf " ... testing '$$file' with argument '$$arg':"; \
	    F="`basename $$file .ml`"; \
	    (OCAMLRUNPARAM=b=1 $(EXECNAME) $$arg || true) \
	         >$$F.$$arg.result 2>&1; \
	    $(DIFF) $$F.$$arg.reference $$F.$$arg.result >/dev/null \
	    && echo " => passed" || echo " => failed"; \
	  done; \
	done

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result program program.exe

include $(BASEDIR)/makefiles/Makefile.common
