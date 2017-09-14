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
	  rm -f log; \
	  ( $(OCAMLOPT) -w a -output-complete-obj -o $${file}.exe.$(O) $${file} >> log 2>&1 && \
	    $(CC) $(CFLAGS) $(CPPFLAGS) -I$(CTOPDIR)/byterun -o $${file}_stub $${file}.exe.$(O) $(NATIVECCLIBS) $${file}_stub.c >> log 2>&1 && \
	    ./$${file}_stub >> log 2>&1 && \
	    echo " => passed") || (echo " => failed" && cat log) ; \
	done
	@for file in *.ml; do \
	  printf " ... testing byte '$$file'"; \
          rm -f log; \
	  ( $(OCAMLC) -ccopt "-I$(CTOPDIR)/byterun" -w a -output-complete-obj -o $${file}.bc.$(O) $${file} >> log 2>&1 && \
	    $(CC) $(CFLAGS) $(CPPFLAGS) -I$(CTOPDIR)/byterun -o $${file}_bc_stub $${file}.bc.$(O) $(BYTECCLIBS) $${file}_stub.c >> log 2>&1 && \
	    ./$${file}_bc_stub >> log 2>&1 && \
	    echo " => passed") || (echo " => failed" && cat log); \
	done
	@rm -f log

promote:

clean: defaultclean
	@rm -f ./a.out

include $(BASEDIR)/makefiles/Makefile.common
