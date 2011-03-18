#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../LICENSE.      #
#                                                                       #
#########################################################################

# $Id$

include Makefile.common

CFLAGS=-DCAML_NAME_SPACE -O $(BYTECCCOMPOPTS) $(IFLEXDIR)
DFLAGS=-DCAML_NAME_SPACE -g -DDEBUG $(BYTECCCOMPOPTS) $(IFLEXDIR)

OBJS=$(COMMONOBJS) unix.o main.o
DOBJS=$(OBJS:.o=.d.o) instrtrace.d.o
PICOBJS=$(OBJS:.o=.pic.o)

SHARED_LIBS_TMP=$(SUPPORTS_SHARED_LIBRARIES:%false=)
SHARED_LIBS_DEPS=$(SHARED_LIBS_TMP:%true=libcamlrun_shared.so)

all:: $(SHARED_LIBS_DEPS)

ocamlrun$(EXE): libcamlrun.a prims.o
	$(MKEXE) $(BYTECCLINKOPTS) -o ocamlrun$(EXE) \
		  prims.o libcamlrun.a $(BYTECCLIBS)

ocamlrund$(EXE): libcamlrund.a prims.o
	$(MKEXE) -g $(BYTECCLINKOPTS) -o ocamlrund$(EXE) \
		  prims.o libcamlrund.a $(BYTECCLIBS)

libcamlrun.a: $(OBJS)
	ar rc libcamlrun.a $(OBJS)
	$(RANLIB) libcamlrun.a

libcamlrund.a: $(DOBJS)
	ar rc libcamlrund.a $(DOBJS)
	$(RANLIB) libcamlrund.a

libcamlrun_shared.so: $(PICOBJS)
	$(MKDLL) -o libcamlrun_shared.so $(PICOBJS) $(BYTECCLIBS)

install::
	if test -f libcamlrun_shared.so; then \
	  cp libcamlrun_shared.so $(LIBDIR)/libcamlrun_shared.so; fi

clean::
	rm -f libcamlrun_shared.so
	rm -f *.pic.c

.SUFFIXES: .d.o .pic.o

.c.d.o:
	ln -s -f $*.c $*.d.c
	$(CC) -c $(DFLAGS) $*.d.c

.c.pic.o:
	ln -s -f $*.c $*.pic.c
	$(CC) -c $(CFLAGS) $(SHAREDCCCOMPOPTS) $*.pic.c

depend : prims.c opnames.h jumptbl.h version.h
	-gcc -MM $(BYTECCCOMPOPTS) *.c > .depend
	-gcc -MM $(BYTECCCOMPOPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' >> .depend
	-gcc -MM $(BYTECCCOMPOPTS) *.c | sed -e 's/\.o/.pic.o/' >> .depend
.PHONY: depend

include .depend
