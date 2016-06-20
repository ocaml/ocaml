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
LIBRARIES=bigarray
CMA=$(LIBRARIES:=.cma)
CMXA=$(LIBRARIES:=.cmxa)
FLAGS=-w A \
-I $(OTOPDIR)/otherlibs/bigarray

EXECNAME=./program

run-all:
	@for file in *.ml; do \
	  printf " ... testing '$$file'"; \
	  F="`basename $$file .ml`"; \
	  $(OCAMLC) $(FLAGS) $(CMA) -o $(EXECNAME) $$file 2>$$F.result; \
	  $(DIFF) $$F.reference $$F.result >/dev/null \
	  &&  printf " ... ocamlopt testing"; \
	  $(OCAMLOPT) $(FLAGS) $(CMXA) -o $(EXECNAME) $$file 2>$$F.result; \
	  $(DIFF) $$F.reference $$F.result >/dev/null \
	  && echo ": => passed" || echo ": => failed"; \
	done;

promote: defaultpromote

clean: defaultclean
	@rm -f *.result $(EXECNAME)

include $(BASEDIR)/makefiles/Makefile.common
