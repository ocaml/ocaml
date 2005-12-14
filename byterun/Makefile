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

include ../config/Makefile

CC=$(BYTECC)
CFLAGS=-DCAML_NAME_SPACE -O $(BYTECCCOMPOPTS)
DFLAGS=-DCAML_NAME_SPACE -g -DDEBUG $(BYTECCCOMPOPTS)

OBJS=interp.o misc.o stacks.o fix_code.o startup.o main.o \
  freelist.o major_gc.o minor_gc.o memory.o alloc.o roots.o globroots.o \
  fail.o signals.o printexc.o backtrace.o \
  compare.o ints.o floats.o str.o array.o io.o extern.o intern.o \
  hash.o sys.o meta.o parsing.o gc_ctrl.o terminfo.o md5.o obj.o \
  lexing.o callback.o debugger.o weak.o compact.o finalise.o custom.o \
  dynlink.o unix.o

DOBJS=$(OBJS:.o=.d.o) instrtrace.d.o

PRIMS=alloc.c array.c compare.c extern.c floats.c gc_ctrl.c hash.c \
  intern.c interp.c ints.c io.c lexing.c md5.c meta.c obj.c parsing.c \
  signals.c str.c sys.c terminfo.c callback.c weak.c finalise.c stacks.c \
  dynlink.c

PUBLIC_INCLUDES=alloc.h callback.h config.h custom.h fail.h intext.h \
  memory.h misc.h mlvalues.h printexc.h signals.h compatibility.h

all: ocamlrun$(EXE) ld.conf

ocamlrun$(EXE): libcamlrun.a prims.o
	$(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) -o ocamlrun$(EXE) \
	          prims.o libcamlrun.a $(BYTECCLIBS)

ocamlrund$(EXE): libcamlrund.a prims.o
	$(BYTECC) -g $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) -o ocamlrund$(EXE) \
	          prims.o libcamlrund.a $(BYTECCLIBS)

install:
	cp ocamlrun$(EXE) $(BINDIR)/ocamlrun$(EXE)
	cp libcamlrun.a $(LIBDIR)/libcamlrun.a
	cd $(LIBDIR); $(RANLIB) libcamlrun.a
	if test -d $(LIBDIR)/caml; then : ; else mkdir $(LIBDIR)/caml; fi
	for i in $(PUBLIC_INCLUDES); do \
          sed -f ../tools/cleanup-header $$i > $(LIBDIR)/caml/$$i; \
        done
	cp ld.conf $(LIBDIR)/ld.conf

ld.conf: ../config/Makefile
	echo "$(STUBLIBDIR)" >ld.conf
	echo "$(LIBDIR)" >>ld.conf

libcamlrun.a: $(OBJS)
	ar rc libcamlrun.a $(OBJS)
	$(RANLIB) libcamlrun.a

libcamlrund.a: $(DOBJS)
	ar rc libcamlrund.a $(DOBJS)
	$(RANLIB) libcamlrund.a

clean:
	rm -f ocamlrun$(EXE) ocamlrund$(EXE) *.o lib*.a
	rm -f primitives prims.c opnames.h jumptbl.h ld.conf

primitives : $(PRIMS)
	sed -n -e "s/CAMLprim value \([a-z0-9_][a-z0-9_]*\).*/\1/p" \
	    $(PRIMS) > primitives

prims.c : primitives
	(echo '#include "mlvalues.h"'; \
	 echo '#include "prims.h"'; \
	 sed -e 's/.*/extern value &();/' primitives; \
	 echo 'c_primitive caml_builtin_cprim[] = {'; \
	 sed -e 's/.*/	&,/' primitives; \
	 echo '	 0 };'; \
	 echo 'char * caml_names_of_builtin_cprim[] = {'; \
	 sed -e 's/.*/	"&",/' primitives; \
	 echo '	 0 };') > prims.c

opnames.h : instruct.h
	sed -e '/\/\*/d' \
	    -e '/^#/d' \
	    -e 's/enum /char * names_of_/' \
	    -e 's/{$$/[] = {/' \
	    -e 's/\([[:upper:]][[:upper:]_0-9]*\)/"\1"/g' instruct.h > opnames.h

# jumptbl.h is required only if you have GCC 2.0 or later
jumptbl.h : instruct.h
	sed -n -e '/^  /s/ \([A-Z]\)/ \&\&lbl_\1/gp' \
	       -e '/^}/q' instruct.h > jumptbl.h

version.h : ../stdlib/sys.ml
	sed -n -e 's/;;//' \
            -e '/let *ocaml_version *= */s//#define OCAML_VERSION /p' \
            <../stdlib/sys.ml >version.h

.SUFFIXES: .d.o

.c.d.o:
	@ if test -f $*.o; then mv $*.o $*.f.o; else :; fi
	$(CC) -c $(DFLAGS) $<
	mv $*.o $*.d.o
	@ if test -f $*.f.o; then mv $*.f.o $*.o; else :; fi

depend : prims.c opnames.h jumptbl.h version.h
	gcc -MM $(BYTECCCOMPOPTS) *.c > .depend
	gcc -MM $(BYTECCCOMPOPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' >> .depend

include .depend
