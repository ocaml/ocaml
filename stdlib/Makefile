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

ROOTDIR ?= ..

include Makefile.shared

allopt-base:
	$(MAKE) stdlib.cmxa std_exit.cmx

allopt: allopt-base

installopt-base:
	$(MKDIR) "$(INSTALL_LIBDIR)"
	cp stdlib.cmxa stdlib.a std_exit.o *.cmx "$(INSTALL_LIBDIR)"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) stdlib.a

installopt: installopt-base

camlheader target_camlheader camlheader_ur \
camlheaderd target_camlheaderd \
camlheaderi target_camlheaderi: \
  header.c $(ROOTDIR)/config/Makefile
	if $(HASHBANGSCRIPTS); then \
	  for suff in '' d i; do \
	    echo '#!$(BINDIR)/ocamlrun'$$suff > camlheader$$suff && \
	    echo '#!$(TARGET_BINDIR)/ocamlrun'$$suff >target_camlheader$$suff; \
	  done && \
	  echo '#!' | tr -d '\012' > camlheader_ur; \
	else \
	  for suff in '' d i; do \
	    $(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) \
	              -DRUNTIME_NAME='"$(BINDIR)/ocamlrun'$$suff'"' \
	              header.c -o tmpheader$(EXE) && \
	    strip tmpheader$(EXE) && \
	    mv tmpheader$(EXE) camlheader$$suff && \
	    $(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) \
	              -DRUNTIME_NAME='"$(TARGET_BINDIR)/ocamlrun'$$suff'"' \
	              header.c -o tmpheader$(EXE) && \
	    strip tmpheader$(EXE) && \
	    mv tmpheader$(EXE) target_camlheader$$suff; \
	  done && \
	  cp camlheader camlheader_ur; \
	fi

.PHONY: all allopt allopt-base install installopt installopt-base clean depend
