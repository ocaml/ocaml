#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

include Makefile.shared

# To make custom toplevels

ocamlmktop: ocamlmktop.tpl ../config/Makefile
	sed -e 's|%%BINDIR%%|$(BINDIR)|' ocamlmktop.tpl > ocamlmktop
	chmod +x ocamlmktop

install::
	cp ocamlmktop "$(INSTALL_BINDIR)"

clean::
	rm -f ocamlmktop
