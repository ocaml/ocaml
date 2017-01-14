#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                          Bernhard Schommer                             *
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


default:
	@$(OCAMLOPT) unknown-file  2>&1 | grep "don't know what to do with unknown-file"\
	> unknown-file.opt.result || true
	@$(OCAMLC) unknown-file 2>&1 | grep "don't know what to do with unknown-file" \
	> unknown-file.byte.result  || true
	@for file in *.opt.reference; do \
	  printf " ... testing '$$file':"; \
	  $(DIFF) $$file `basename $$file reference`result >/dev/null \
          && echo " => passed" || echo " => failed"; \
	done
	@for file in *.byte.reference; do \
	  printf " ... testing '$$file':"; \
	  $(DIFF) $$file `basename $$file reference`result >/dev/null \
          && echo " => passed" || echo " => failed"; \
	done

promote: defaultpromote

clean: defaultclean
	@rm -f *.result

include $(BASEDIR)/makefiles/Makefile.common
