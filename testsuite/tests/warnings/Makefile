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
FLAGS=-w A
EXECNAME=./program

run-all:
	@for file in *.ml; do \
	  printf " ... testing '$$file':"; \
	  $(OCAMLC) $(FLAGS) -o $(EXECNAME) $$file 2> `basename $$file ml`result; \
	  $(DIFF) `basename $$file ml`reference `basename $$file ml`result > /dev/null && echo " => passed" || echo " => failed"; \
	done;

promote: defaultpromote

clean: defaultclean
	@rm -f *.result $(EXECNAME)

include $(BASEDIR)/makefiles/Makefile.common
