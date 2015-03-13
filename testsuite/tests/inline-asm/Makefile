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

CASES=float_round

default: run-byte run-native compare-asm

include $(BASEDIR)/makefiles/Makefile.common

.SUFFIXES: .byte$(EXE) .c .ml .native$(EXE) .o

$(CASES:=.ml): %: $(ARCH)/%
	@ln -s $< $@

$(CASES:=_stubs.c): %: $(ARCH)/%
	@ln -s $< $@

%.byte$(EXE): %.ml %_stubs.o
	@$(OCAMLC) -custom -o $@ $(<:.ml=_stubs.c) $<

.ml.native$(EXE):
	@$(OCAMLOPT) -S -o $@ $(<:.ml=_stubs.c) $<

.s.exe:

.PHONY: run-byte
run-byte: $(CASES:=.byte$(EXE))
	@for file in $<; do \
	  printf " ... testing '$$file'"; \
	  ./$$file \
	  && echo " => passed" || echo " => failed"; \
	done

.PHONY: run-native
run-native: $(CASES:=.native$(EXE))
	@for file in $<; do \
	  printf " ... testing '$$file'"; \
	  ./$$file \
	  && echo " => passed" || echo " => failed"; \
	done

.PHONY: compare-asm
compare-asm: $(CASES:=.s)
	@for file in $<; do \
	  printf " ... verifying '$$file'"; \
	  cmp -s $$file $(ARCH)/$$file \
	  && echo " => passed" || echo " => failed"; \
	done

promote:

.PHONY: clean
clean: defaultclean
	@rm -f *.byte$(EXE) *.c *.ml *.native$(EXE) *.s
