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

# Makefile for the str library

LIBNAME=runtime_events
COBJS=runtime_events_consumer.$(O) runtime_events.$(O)
CLIBNAME=camlruntime_events
CAMLOBJS=runtime_events.cmo

include ../Makefile.otherlibs.common
runtime_events.cmo: runtime_events.cmi
runtime_events.cmx: runtime_events.cmi

.PHONY: depend
depend:
	$(OCAMLRUN) $(ROOTDIR)/boot/ocamlc -depend -slash *.mli *.ml > .depend

include .depend