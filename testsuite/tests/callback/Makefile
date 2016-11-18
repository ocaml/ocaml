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
LD_PATH=$(TOPDIR)/otherlibs/unix

.PHONY: default
default:
	@case " $(OTHERLIBRARIES) " in \
	  *' unix '*) $(SET_LD_PATH) $(MAKE) run-byte; \
	              $(SET_LD_PATH) $(MAKE) run-opt;; \
	esac


.PHONY: run-byte
run-byte: 
	@echo " ... testing 'bytecode':"
	@for f in *.ml; do \
	  F=`basename $$f .ml`; \
		$(CC) -c $$F.c; \
		$(OCAMLC) $(COMPFLAGS) -c $$F.mli; \
		$(OCAMLC) $(COMPFLAGS) -c $$F.ml; \
		$(OCAMLC) $(COMPFLAGS) -o ./$$F.byte$(EXE) -custom unix.cma $$F.$(O) $$F.cmo; \
		./$$F.byte$(EXE) > $$F.byte.result; \
		$(DIFF) $$F.reference $$F.byte.result && echo " => passed" || echo " => failed"; \
	done

.PHONY: run-opt
run-opt: 
	@if $(BYTECODE_ONLY); then : ; else \
		echo " ... testing 'nativecode':"; \
		for f in *.ml; do \
			F=`basename $$f .ml`; \
			$(CC) -c $$F.c; \
			mv $$F.$(O) $$F.c.$(O); \
			$(OCAMLOPT) $(COMPFLAGS) -c $$F.mli; \
			$(OCAMLOPT) $(COMPFLAGS) -o ./$$F.native$(EXE) unix.cmxa $$F.c.$(O) $$F.ml; \
			./$$F.native$(EXE) > $$F.native.result; \
			$(DIFF) $$F.reference $$F.native.result && echo " => passed" || echo " => failed"; \
		done; \
	fi

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f *.result *.byte$(EXE) *.native$(EXE)

include $(BASEDIR)/makefiles/Makefile.common
