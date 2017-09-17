#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                 Xavier Clerc, SED, INRIA Rocquencourt                  *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR=../..
SHOULD_FAIL=

compile:
	@for file in *.ml; do \
	  printf " ... testing native '$$file'"; \
          if $(BYTECODE_ONLY); then \
	    echo " => skipped"; \
	  else \
	    rm -f log $${file}.exe.$(O) $${file}_stub$(EXE); \
	    ( $(OCAMLOPT) -w a -output-complete-obj -o $${file}.exe.$(O) \
                          $${file} >> log 2>&1 && \
	      $(MKEXE) -I$(CTOPDIR)/byterun -o $${file}_stub$(EXE) \
                       $${file}.exe.$(O) $(NATIVECCLIBS) $${file}_stub.c \
                       >> log 2>&1 && \
	      ./$${file}_stub$(EXE) >> log 2>&1 && \
	      echo " => passed") || (echo " => failed" && cat log) ; \
	  fi \
	done
	@for file in *.ml; do \
	  printf " ... testing byte '$$file'"; \
	  if [ $(TOOLCHAIN) = msvc ]; then \
	    echo " => skipped"; \
	  else \
            rm -f log $${file}.bc.$(O) $${file}_bc_stub$(EXE); \
	    ( $(OCAMLC) -ccopt "-I$(CTOPDIR)/byterun" -w a -output-complete-obj\
                        -o $${file}.bc.$(O) $${file} >> log 2>&1 && \
	      $(MKEXE) -I$(CTOPDIR)/byterun -o $${file}_bc_stub$(EXE) \
                       $${file}.bc.$(O) $(BYTECCLIBS) $${file}_stub.c \
                       >> log 2>&1 && \
	      ./$${file}_bc_stub$(EXE) >> log 2>&1 && \
	      echo " => passed") || (echo " => failed" && cat log); \
	  fi; \
	done
	@rm -f log

promote:

clean: defaultclean
	@rm -f ./a.out

include $(BASEDIR)/makefiles/Makefile.common
