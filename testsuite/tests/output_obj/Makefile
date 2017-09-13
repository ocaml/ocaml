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
	  ( $(OCAMLOPT) -w a -output-complete-obj -o $${file}.exe.o $${file} 2>/dev/null && \
	    $(CC) $(CFLAGS) $(CPPFLAGS) -I$(CTOPDIR)/byterun -o $${file}_stub $${file}.exe.o -lm -ldl $${file}_stub.c 2>/dev/null && \
	    ./$${file}_stub 1>/dev/null 2>/dev/null && \
	    echo " => passed" ) || echo " => failed"; \
	done
	@for file in *.ml; do \
	  printf " ... testing byte '$$file'"; \
	  ( $(OCAMLC) -ccopt "-I$(CTOPDIR)/byterun" -w a -output-complete-obj -o $${file}.bc.o $${file} 2>/dev/null && \
	    $(CC) $(CFLAGS) $(CPPFLAGS) -I$(CTOPDIR)/byterun -o $${file}_bc_stub $${file}.bc.o -lm -ldl -lcurses $${file}_stub.c 2>/dev/null && \
	    ./$${file}_bc_stub 1>/dev/null 2>/dev/null && \
	    echo " => passed" ) || echo " => failed"; \
	done

promote:

clean: defaultclean
	@rm -f ./a.out

include $(BASEDIR)/makefiles/Makefile.common
