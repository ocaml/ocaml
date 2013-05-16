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

CC=$(NATIVECC) -I $(CTOPDIR)/byterun

COMPFLAGS=-I $(OTOPDIR)/otherlibs/unix

.PHONY: default
default:
	@case " $(OTHERLIBRARIES) " in \
	  *' unix '*) LD_LIBRARY_PATH='$(OTOPDIR)/otherlibs/unix' \
	                $(MAKE) run-byte run-opt;; \
	esac

.PHONY: common
common:
	@$(CC) -c callbackprim.c

.PHONY: run-byte
run-byte: common
	@printf " ... testing 'bytecode':"
	@$(OCAMLC) $(COMPFLAGS) -c tcallback.ml
	@$(OCAMLC) $(COMPFLAGS) -o ./program -custom unix.cma \
	           callbackprim.$(O) tcallback.cmo
	@./program >bytecode.result
	@$(DIFF) reference bytecode.result \
	&& echo " => passed" || echo " => failed"

.PHONY: run-opt
run-opt: common
	@if $(BYTECODE_ONLY); then : ; else \
	  printf " ... testing 'native':"; \
	  $(OCAMLOPT) $(COMPFLAGS) -c tcallback.ml; \
	  $(OCAMLOPT) $(COMPFLAGS) -o ./program unix.cmxa callbackprim.$(O) \
	              tcallback.cmx; \
	  ./program >native.result; \
	  $(DIFF) reference native.result \
	  && echo " => passed" || echo " => failed"; \
	fi

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result ./program

include $(BASEDIR)/makefiles/Makefile.common
