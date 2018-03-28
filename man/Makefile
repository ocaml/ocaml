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

include ../config/Makefile

INSTALL_DIR=$(DESTDIR)$(MANDIR)/man$(PROGRAMS_MAN_SECTION)

install:
	for i in *.m; do cp \
	  $$i $(INSTALL_DIR)/`basename $$i .m`.$(PROGRAMS_MAN_SECTION); done
	echo '.so man$(PROGRAMS_MAN_SECTION)/ocamlc.$(PROGRAMS_MAN_SECTION)' \
	     > $(INSTALL_DIR)/ocamlc.opt.$(PROGRAMS_MAN_SECTION)
	echo '.so man$(PROGRAMS_MAN_SECTION)/ocamlopt.$(PROGRAMS_MAN_SECTION)' \
	     > $(INSTALL_DIR)/ocamlopt.opt.$(PROGRAMS_MAN_SECTION)
	echo '.so man$(PROGRAMS_MAN_SECTION)/ocamlcp.$(PROGRAMS_MAN_SECTION)' \
	     > $(INSTALL_DIR)/ocamloptp.$(PROGRAMS_MAN_SECTION)
