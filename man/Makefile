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

ROOTDIR = ..
SUBDIR_NAME = man
include $(ROOTDIR)/Makefile.common

MANPAGES = $(addsuffix .1,\
  ocaml ocamlc ocamlc.opt ocamlcp ocamldebug ocamldep ocamldoc ocamllex \
  ocamlmktop ocamlopt ocamlopt.opt ocamloptp ocamlprof ocamlrun ocamlyacc)

.PHONY: install
install:
	$(call INSTALL_ITEMS, $(MANPAGES), man, $(INSTALL_MANDIR_PROGRAMS))
