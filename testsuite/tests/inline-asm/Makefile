#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#                     Vladimir Brankov, Jane Street                     #
#                                                                       #
#   Copyright 2015 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

BASEDIR=../..

BYTE_CASES=float_round
CASES=$(BYTE_CASES) comprehensive test_intrinsics

default: run-byte run-native compare-asm

include $(BASEDIR)/makefiles/Makefile.common

.SUFFIXES: .byte$(EXE) .c .ml .native$(EXE)
.PRECIOUS: %.c %.ml %.o

%.ml: $(ARCH)/%.ml
	@ln -s $< $@

%.c: $(ARCH)/%.c
	@ln -s $< $@

%.o: %.c
	@$(OCAMLC) $^

%.byte$(EXE): %.ml %_stubs.o
	@$(OCAMLC) -custom -o $@ $^

%.native$(EXE): intrinsics.ml %.ml
	@$(OCAMLOPT) -S -inline 20 -o $@ $^

.exe.s:

.PHONY: run-byte
run-byte: $(BYTE_CASES:=.byte$(EXE))
	@for file in $^; do \
	  printf " ... testing '$$file'"; \
	  ./$$file \
	  && echo " => passed" || echo " => failed"; \
	done

.PHONY: run-native
run-native: $(CASES:=.native$(EXE))
	@for file in $^; do \
	  printf " ... testing '$$file'"; \
	  ./$$file \
	  && echo " => passed" || echo " => failed"; \
	done

.PHONY: compare-asm
compare-asm: $(CASES:=.s)
	@for file in $^; do \
	  printf " ... verifying '$$file'"; \
	  cmp -s $$file $(ARCH)/$$file \
	  && echo " => passed" || echo " => failed"; \
	done

promote:

.PHONY: clean
clean: defaultclean
	@rm -f *.byte$(EXE) *.c *.ml *.native$(EXE) *.s
