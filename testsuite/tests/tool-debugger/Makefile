#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Damien Doligez, EPI Gallium, INRIA Rocquencourt            #
#                                                                       #
#   Copyright 2013 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

BASEDIR=../..
MAIN_MODULE=debuggee
ADD_COMPFLAGS=-g

.PHONY: default
default:
	@$(MAKE) compile
	@$(SET_LD_PATH) $(MAKE) run

.PHONY: compile
compile: $(ML_FILES) $(CMO_FILES) $(MAIN_MODULE).cmo
	@rm -f program.byte program.byte.exe
	@$(OCAMLC) $(ADD_COMPFLAGS) $(ADD_CFLAGS) -o program.byte$(EXE) \
	           $(O_FILES) $(CMA_FILES) $(CMO_FILES) $(ADD_CMO_FILES) \
	           $(MAIN_MODULE).cmo

.PHONY: run
run:
	@printf " ... testing with ocamlc"
	@echo 'source input_script' | \
	 $(MYRUNTIME) $(TOPDIR)/debugger/ocamldebug program.byte$(EXE) 2>&1 | \
	 grep -v 'Debugger version\|^Time:' >$(MAIN_MODULE).result \
	 && $(DIFF) $(MAIN_MODULE).reference $(MAIN_MODULE).result >/dev/null \
	 && echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result program.byte program.byte.exe \
	       program.native program.native.exe \
	       $(GENERATED_SOURCES) $(O_FILES) $(TEST_TEMP_FILES)

include $(BASEDIR)/makefiles/Makefile.common
