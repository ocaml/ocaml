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
DFLAGS=-DCAML_NAME_SPACE -g -DDEBUG $(BYTECCCOMPOPTS)

OBJS=$(COMMONOBJS) unix.o main.o
DOBJS=$(OBJS:.o=.d.o) instrtrace.d.o

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

.SUFFIXES: .d.o

.c.d.o:
	@ if test -f $*.o; then mv $*.o $*.f.o; else :; fi
	$(CC) -c $(DFLAGS) $<
	mv $*.o $*.d.o
	@ if test -f $*.f.o; then mv $*.f.o $*.o; else :; fi

depend : prims.c opnames.h jumptbl.h version.h
	-gcc -MM $(BYTECCCOMPOPTS) *.c > .depend
	-gcc -MM $(BYTECCCOMPOPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' >> .depend
.PHONY: depend

include .depend
