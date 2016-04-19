#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                     Thomas Refis, Jane Street Europe                   *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*   Copyright 2016 Jane Street Group LLC                                 *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

default:
	printf " ... testing 'test.reference':"
	@$(OCAMLOPT) -c submodule.ml
	@$(OCAMLOPT) -c aliases.ml
	@$(OCAMLOPT) -c test.ml
	@$(OCAMLOPT) -a submodule.cmx aliases.cmx -o mylib.cmxa
	@$(OCAMLOPT) mylib.cmxa test.cmx -o test.native
	@./test.native > test.result
	@$(DIFF) test.result test.reference >/dev/null \
	    && echo " => passed" || echo " => failed"

promote: defaultpromote

clean: defaultclean
	@rm -f *.result
	@rm -f test.native

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.common
COMPFLAGS = -no-alias-deps
