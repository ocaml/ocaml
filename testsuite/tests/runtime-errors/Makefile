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
	@if [ ! `grep -c HAS_STACK_OVERFLOW_DETECTION ../../../config/s.h` ]; then \
	  test -z "$(BYTECODE_ONLY)" && rm -f stackoverflow.byte stackoverflow.native || true; \
	fi

run:
	@ulimit -s 1024; \
         for f in *.bytecode; do \
	  printf " ... testing '$$f':"; \
	  (./$$f > $$f.result 2>&1; true); \
	  $(DIFF) $$f.reference $$f.result > /dev/null || (echo " => failed" && exit 1) && echo " => passed"; \
	  if [ -z "$(BYTECODE_ONLY)" ]; then \
	    printf " ... testing '`basename $$f bytecode`native':"; \
	    (./`basename $$f bytecode`native > `basename $$f bytecode`native.result 2>&1; true); \
	    $(DIFF) `basename $$f bytecode`native.reference `basename $$f bytecode`native.result > /dev/null || (echo " => failed" && exit 1) && echo " => passed"; \
	  fi; \
	done

promote: defaultpromote

clean: defaultclean
	@rm -f *.bytecode *.native *.result

include $(BASEDIR)/makefiles/Makefile.common
