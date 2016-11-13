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
GENERATED= a.ml b.ml c.ml

default: pr7325

pr7325:
	@printf " ... testing pr7325:"
	@echo "type _ t = T" > a.ml
	@echo "type 'a t = 'a A.t" > b.ml
	@echo 'external f : unit -> unit B.t = "%identity"' > c.ml
	@$(OCAMLC) -c a.ml b.ml && rm a.cmi && $(OCAMLC) -c c.ml \
	  && echo " => passed" || echo " => failed"

clean: defaultclean
	@rm -f $(GENERATED)

include $(BASEDIR)/makefiles/Makefile.common
