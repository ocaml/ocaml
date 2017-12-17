#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*           Xavier Leroy, projet Cristal, INRIA Rocquencourt             *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Makefile for Raw_spacetime_lib

LIBNAME=raw_spacetime_lib
COBJS=spacetime_offline.$(O)
CAMLOBJS=raw_spacetime_lib.cmo

include ../Makefile

.PHONY: depend
depend:
ifeq "$(TOOLCHAIN)" "msvc"
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
	$(CC) -MM $(CPPFLAGS) *.c | sed -e 's/\.o/.$$(O)/g' > .depend
	$(CAMLRUN) $(ROOTDIR)/tools/ocamldep -slash *.mli *.ml >> .depend
endif

include .depend
