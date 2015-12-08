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

BINARIES := write32.byte write32.native read32.byte read32.native \
	    write64.byte write64.native read64.byte read64.native

BASEDIR=../..

.PHONY: default
default:
	rm -f $(BINARIES)
	@$(MAKE) compile
	@$(MAKE) run

.PHONY: compile
compile: $(BINARIES)

.PHONY: run
run:
	@for arch in 32 64; do						\
	  for kind in byte native; do					\
	    echo -n  " ... testing 'arch-$$arch/$$kind'";		\
	    rm -f data;                             			\
	    ./write$$arch.$$kind                 			\
	      > write$$arch$$kind.out 2> write$$arch$$kind.err		\
	      || exit 1;						\
	    ./read$$arch.$$kind	                                       	\
	      > read$$arch$$kind.out 2> read$$arch$$kind.err		\
	      && ( echo " => passed" )					\
	      || ( echo " => failed" )					\
	  done;								\
	done

###

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f data *.byte *.native *.out *.err

###

%.byte: %.ml
	@$(OCAMLC) $< -custom -o $@

%.native: %.ml
	@$(OCAMLOPT) $< -o $@

###

include $(BASEDIR)/makefiles/Makefile.common
