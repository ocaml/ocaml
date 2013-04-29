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

compile: tscanf2_io.cmo
	@$(OCAMLC) unix.cma tscanf2_io.cmo -o master.byte tscanf2_master.ml
	@$(OCAMLC) tscanf2_io.cmo -o slave.byte tscanf2_slave.ml
	@if [ -z "$(BYTECODE_ONLY)" ]; then \
	  $(MAKE) tscanf2_io.cmx; \
	  $(OCAMLOPT) unix.cmxa tscanf2_io.cmx -o master.native tscanf2_master.ml; \
	  $(OCAMLOPT) tscanf2_io.cmx -o slave.native tscanf2_slave.ml; \
	fi

run:
	@printf " ... testing with ocamlc"
	@./master.byte "`$(CYGPATH) ./slave.byte`" > result.byte 2>&1
	@$(DIFF) reference result.byte > /dev/null || (echo " => failed" && exit 1)
	@if [ -z "$(BYTECODE_ONLY)" ]; then \
	  printf " ocamlopt" && \
	  ./master.native "`$(CYGPATH) ./slave.native`" > result.native 2>&1 && \
	  $(DIFF) reference result.native > /dev/null || (echo " => failed" && exit 1) \
	fi
	@echo " => passed"

promote:
	@cp result.byte reference

clean: defaultclean
	@rm -f master.* slave.* result.*

include $(BASEDIR)/makefiles/Makefile.common
