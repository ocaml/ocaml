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


.PHONY: run
run: *.txt
	@for file in *_fail.txt; do \
		printf " ...testing '$$file'";\
		$(OCAML) -args $$file 2> /dev/null \
		&& echo " => failed" || echo " => passed";\
	done

.PHONY: promote
promote: defaultpromote


include $(BASEDIR)/makefiles/Makefile.common
