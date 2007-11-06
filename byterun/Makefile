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

CFLAGS=-DCAML_NAME_SPACE -O $(BYTECCCOMPOPTS) $(FLEXLINK)
DFLAGS=-DCAML_NAME_SPACE -g -DDEBUG $(BYTECCCOMPOPTS)

OBJS=$(COMMONOBJS) unix.o main.o 
DOBJS=$(OBJS:.o=.d.o) instrtrace.d.o

ocamlrun$(EXE): libcamlrun.a prims.o
	$(call MKEXE,ocamlrun$(EXE),main.o prims.o libcamlrun.a $(BYTECCLIBS), \
	$(BYTECCCOMPOPTS) $(BYTECCLINKOPTS))

ocamlrund$(EXE): libcamlrund.a prims.o
	$(BYTECC) -g $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) -o ocamlrund$(EXE) \
	          prims.o libcamlrund.a $(BYTECCLIBS)

libcamlrun.$(A): $(OBJS)
	$(call MKLIB,libcamlrun.$(A),$(OBJS))


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

include .depend
