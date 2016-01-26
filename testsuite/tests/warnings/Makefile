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

run-all:
	@$(OCAMLC) $(FLAGS) -c deprecated_module.mli
	@$(OCAMLC) $(FLAGS) -c module_without_cmx.mli
	@for file in *.ml; do \
	  printf " ... testing '$$file':"; \
	  F="`basename $$file .ml`"; \
	  $(OCAMLC) $(FLAGS) -c $$file 2>$$F.result; \
	  $(DIFF) $$F.reference $$F.result >/dev/null \
	  && echo " => passed" || echo " => failed"; \
	done;
	@for file in *.opt.ml; do \
	  printf " ... testing '$$file' with ocamlopt:"; \
	  if $(BYTECODE_ONLY); then echo " => skipped"; else \
	    F="`basename $$file .ml`"; \
	    $(OCAMLOPT) $(FLAGS) -c $$file 2>$$F.opt_result; \
	    $(DIFF) $$F.opt_reference $$F.opt_result >/dev/null \
	    && echo " => passed" || echo " => failed"; \
	  fi \
	done;

promote: defaultpromote

clean: defaultclean
	@rm -f *.result *.opt_result

include $(BASEDIR)/makefiles/Makefile.common
