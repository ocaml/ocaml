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

CFLAGS=$(BYTECCCOMPOPTS) $(IFLEXDIR)
DFLAGS=-g -DDEBUG $(BYTECCCOMPOPTS) $(IFLEXDIR)
IFLAGS=$(CFLAGS) -DCAML_INSTR

OBJS=$(COMMONOBJS) $(UNIX_OR_WIN32).o main.o
DOBJS=$(OBJS:.o=.d.o) instrtrace.d.o
IOBJS=$(OBJS:.o=.i.o)
PICOBJS=$(OBJS:.o=.pic.o)

all:: all-$(SHARED)

ocamlrun$(EXE): libcamlrun.a prims.o
	$(MKEXE) $(BYTECCLINKOPTS) -o ocamlrun$(EXE) \
		  prims.o libcamlrun.a $(BYTECCLIBS)

ocamlrund$(EXE): libcamlrund.a prims.o
	$(MKEXE) $(MKEXEDEBUGFLAG) $(BYTECCLINKOPTS) -o ocamlrund$(EXE) \
		  prims.o libcamlrund.a $(BYTECCLIBS)

ocamlruni$(EXE): prims.o libcamlruni.a
	$(MKEXE) $(BYTECCLINKOPTS) -o $@ $^ $(BYTECCLIBS)

libcamlrun.a: $(OBJS)
	$(ARCMD) rc libcamlrun.a $(OBJS)
	$(RANLIB) libcamlrun.a

libcamlrund.a: $(DOBJS)
	$(ARCMD) rc libcamlrund.a $(DOBJS)
	$(RANLIB) libcamlrund.a

libcamlruni.a: $(IOBJS)
	$(ARCMD) rc $@ $^
	$(RANLIB) $@

all-noshared:
.PHONY: all-noshared

all-shared: libcamlrun_pic.a libcamlrun_shared.so
.PHONY: all-shared

libcamlrun_pic.a: $(PICOBJS)
	$(ARCMD) rc libcamlrun_pic.a $(PICOBJS)
	$(RANLIB) libcamlrun_pic.a

libcamlrun_shared.so: $(PICOBJS)
	$(MKDLL) -o libcamlrun_shared.so $(PICOBJS) $(BYTECCLIBS)

install:: install-$(SHARED)

install-noshared:
.PHONY: install-noshared

install-shared:
	cp libcamlrun_shared.so "$(INSTALL_LIBDIR)/libcamlrun_shared.so"
	cp libcamlrun_pic.a "$(INSTALL_LIBDIR)/libcamlrun_pic.a"
	cd "$(INSTALL_LIBDIR)"; $(RANLIB) libcamlrun_pic.a
.PHONY: install-shared

clean::
	rm -f libcamlrun_shared.so libcamlrun_pic.a

%.d.o: %.c
	$(CC) -c $(DFLAGS) $< -o $@

%.i.o: %.c
	$(CC) -c $(IFLAGS) -o $@ $<

%.pic.o: %.c
	$(CC) -c $(CFLAGS) $(SHAREDCCCOMPOPTS) $< -o $@

depend : prims.c caml/opnames.h caml/jumptbl.h caml/version.h
	-$(CC) -MM $(BYTECCCOMPOPTS) *.c > .depend
	-$(CC) -MM $(BYTECCCOMPOPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' \
	       >> .depend
	-$(CC) -MM $(BYTECCCOMPOPTS) -DCAML_INSTR *.c | sed -e 's/\.o/.i.o/' \
	       >> .depend
	-$(CC) -MM $(BYTECCCOMPOPTS) *.c | sed -e 's/\.o/.pic.o/' >> .depend
.PHONY: depend

include .depend
