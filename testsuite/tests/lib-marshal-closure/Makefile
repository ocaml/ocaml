#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#                          Benoit Vaugon, ENSTA                         #
#                                                                       #
#   Copyright 2014 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

BYTECODE_EXES := write32.byte read32.byte write32.cbyte read32.cbyte	\
		 write64.byte read64.byte write64.cbyte read64.cbyte
NATIVE_EXES := write32.native read32.native write64.native read64.native
EXES := $(BYTECODE_EXES) $(NATIVE_EXES)

BYTECODE_RUNS := run-32.byte run-32.cbyte run-64.byte run-64.cbyte
NATIVE_RUNS := run-32.native run-64.native

BASEDIR=../..

.PHONY: default
default:
	@$(MAKE) clean
	@$(MAKE) compile
	@$(MAKE) run

###

.PHONY: compile
compile:
	@if $(NATIVECODE_ONLY); then : ; else $(MAKE) $(BYTECODE_EXES); fi
	@if $(BYTECODE_ONLY); then : ; else $(MAKE) $(NATIVE_EXES); fi

%.byte: %.ml
	@$(OCAMLC) $< -o $@

%.cbyte: %.ml
	@$(OCAMLC) $< -custom -o $@

%.native: %.ml
	@$(OCAMLOPT) $< -o $@

###

.PHONY: run
run:
	@if $(NATIVECODE_ONLY); then : ; else $(MAKE) $(BYTECODE_RUNS); fi
	@if $(BYTECODE_ONLY); then : ; else $(MAKE) $(NATIVE_RUNS); fi

run-%.byte:
	@printf " ... testing '$*.byte'"
	@rm -f data
	@$(OCAMLRUN) ./write$*.byte > write$*.byte.out 2> write$*.byte.err
	@$(OCAMLRUN) ./read$*.byte > read$*.byte.out 2> read$*.byte.err	\
	  && echo " => passed" || echo " => failed"

run-%:
	@printf " ... testing '$*'"
	@rm -f data
	@./write$* > write$*.out 2> write$*.err
	@./read$* > read$*.out 2> read$*.err \
	  && echo " => passed" || echo " => failed"

###

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f data *.byte *.cbyte *.native *.out *.err

###

include $(BASEDIR)/makefiles/Makefile.common
