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
include $(ROOTDIR)/Makefile.common

MANPAGES = $(addsuffix .1,\
  ocaml ocamlc ocamlc.opt ocamlcp ocamldebug ocamldep ocamldoc ocamllex \
  ocamlmktop ocamlopt ocamlopt.opt ocamloptp ocamlprof ocamlrun ocamlyacc)

.PHONY: install
install:
	$(MKDIR) '$(INSTALL_PROGRAMS_MAN_DIR)'
	$(INSTALL_DATA) $(MANPAGES) '$(INSTALL_PROGRAMS_MAN_DIR)'
