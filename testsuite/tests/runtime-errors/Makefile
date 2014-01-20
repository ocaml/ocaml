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
	@for f in *.ml; do \
	  $(OCAMLC) -w a -o `basename $$f ml`bytecode $$f; \
	  test -z "$(BYTECODE_ONLY)" && $(OCAMLOPT) -w a -o `basename $$f ml`native $$f || true; \
	done
	@if [ `grep -c HAS_STACK_OVERFLOW_DETECTION ../../../config/s.h` -eq 0 ]; then \
	  rm -f stackoverflow.native || true; \
	fi

run:
	@ulimit -s 1024; \
	 for f in *.bytecode; do \
	  printf " ... testing '$$f':"; \
	  (./$$f > $$f.result 2>&1 || true); \
	  DIFF="$(DIFF)" sh $$f.checker && echo " => passed" || echo " => failed"; \
	  fn=`basename $$f bytecode`native; \
	  if [ -z "$(BYTECODE_ONLY)" -a -f "$$fn" ]; then \
	    printf " ... testing '$$fn':"; \
	    (./$$fn > $$fn.result 2>&1 || true); \
	    DIFF="$(DIFF)" sh $$fn.checker && echo " => passed" || echo " => failed"; \
	  fi; \
	done

promote: defaultpromote

clean: defaultclean
	@rm -f *.bytecode *.native *.result

include $(BASEDIR)/makefiles/Makefile.common
