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

ocamlrun$(EXE): libcamlrun.a prims.o
	$(MKEXE) $(BYTECCLINKOPTS) -o ocamlrun$(EXE) \
		  prims.o libcamlrun.a $(BYTECCLIBS)

ocamlrund$(EXE): libcamlrund.a prims.o
	$(MKEXE) $(MKEXEDEBUGFLAG) $(BYTECCLINKOPTS) -o ocamlrund$(EXE) \
		  prims.o libcamlrund.a $(BYTECCLIBS)

ocamlruni$(EXE): prims.o libcamlruni.a
	$(MKEXE) $(BYTECCLINKOPTS) -o $@ $^ $(BYTECCLIBS)

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
