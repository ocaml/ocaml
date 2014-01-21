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

default: compile run

compile:
	@$(OCAMLC) -ccopt -I -ccopt $(TOPDIR)/byterun cmstub.c
	@$(OCAMLC) -ccopt -I -ccopt $(TOPDIR)/byterun cmmain.c
	@$(OCAMLC) -c cmcaml.ml
	@$(OCAMLC) -custom -o program cmstub.o cmcaml.cmo cmmain.o

run:
	@printf " ... testing 'cmmain':"
	@./program > program.result
	@$(DIFF) program.reference program.result >/dev/null \
	&& echo " => passed" || echo " => failed"

promote: defaultpromote

clean: defaultclean
	@rm -f *.result ./program

include $(BASEDIR)/makefiles/Makefile.common
