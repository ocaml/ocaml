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

IFLAGS=$(CFLAGS) -DCAML_INSTR

ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
all:: libcamlrun_pic.a libcamlrun_shared.so
endif

LIBS = $(BYTECCLIBS)

ocamlrun$(EXE): prims.$(O) libcamlrun.$(A)
	$(MKEXE) $(BYTECCLINKOPTS) -o $@ $^ $(LIBS)

ocamlrund$(EXE): prims.$(O) libcamlrund.$(A)
	$(MKEXE) $(MKEXEDEBUGFLAG) $(BYTECCLINKOPTS) -o $@ $^ $(LIBS)

ocamlruni$(EXE): prims.$(O) libcamlruni.$(A)
	$(MKEXE) $(BYTECCLINKOPTS) -o $@ $^ $(LIBS)

install::
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
	cp libcamlrun_shared.so "$(INSTALL_LIBDIR)"
	cp libcamlrun_pic.a "$(INSTALL_LIBDIR)"
endif

clean::
	rm -f libcamlrun_shared.so libcamlrun_pic.a

%.i.o: %.c
	$(CC) -c $(IFLAGS) -o $@ $<

%.pic.o: %.c
	$(CC) -c $(CFLAGS) $(SHAREDCCCOMPOPTS) $< -o $@

.PHONY: depend
depend : prims.c caml/opnames.h caml/jumptbl.h caml/version.h
	-$(CC) -MM $(BYTECCCOMPOPTS) *.c > .depend
	-$(CC) -MM $(BYTECCCOMPOPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' \
	       >> .depend
	-$(CC) -MM $(BYTECCCOMPOPTS) -DCAML_INSTR *.c | sed -e 's/\.o/.i.o/' \
	       >> .depend
	-$(CC) -MM $(BYTECCCOMPOPTS) *.c | sed -e 's/\.o/.pic.o/' >> .depend

include .depend
