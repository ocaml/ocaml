include ../config/Makefile

CC=$(BYTECC)
CFLAGS=-O $(BYTECCCOMPOPTS)
DFLAGS=-g -DDEBUG $(BYTECCCOMPOPTS)

OBJS=interp.o misc.o stacks.o fix_code.o startup.o main.o \
  freelist.o major_gc.o minor_gc.o memory.o alloc.o roots.o \
  fail.o signals.o printexc.o \
  compare.o ints.o floats.o str.o array.o io.o extern.o intern.o \
  hash.o sys.o meta.o parsing.o gc_ctrl.o terminfo.o md5.o obj.o \
  lexing.o

DOBJS=$(OBJS:.o=.d.o) instrtrace.d.o

PRIMS=array.c compare.c extern.c floats.c gc_ctrl.c hash.c \
  intern.c interp.c ints.c io.c lexing.c md5.c meta.c obj.c parsing.c \
  signals.c str.c sys.c terminfo.c

all: ocamlrun

ocamlrun: $(OBJS) prims.o
	$(BYTECC) $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) -o ocamlrun prims.o $(OBJS) $(CCLIBS)

ocamlrund: $(DOBJS) prims.o
	$(BYTECC) -g $(BYTECCCOMPOPTS) $(BYTECCLINKOPTS) -o ocamlrund prims.o $(DOBJS) $(CCLIBS)

install:
	cp ocamlrun $(BINDIR)/ocamlrun
	rm -f $(LIBDIR)/libcamlrun.a
	ar rc $(LIBDIR)/libcamlrun.a $(OBJS)
	cd $(LIBDIR); $(RANLIB) libcamlrun.a
	if test -d $(LIBDIR)/caml; then : ; else mkdir $(LIBDIR)/caml; fi
	cp mlvalues.h alloc.h misc.h $(LIBDIR)/caml
	sed -e '/#include ".*\/m.h/r ../config/m.h' \
            -e '/#include ".*\/s.h/r ../config/s.h' \
            -e '/#include "/d' config.h > $(LIBDIR)/caml/config.h
	sed -e '/#include ".*gc\.h"/d' \
	    -e '/#define Alloc_small/,/^}/d' \
	    -e '/Modify/,/^}/d' memory.h > $(LIBDIR)/caml/memory.h

libcamlrund.a: $(DOBJS)
	ar rc libcamlrund.a $(DOBJS)

clean:
	rm -f ocamlrun ocamlrund *.o lib*.a
	rm -f primitives prims.c opnames.h jumptbl.h

primitives : $(PRIMS)
	sed -n -e '/\/\* ML \*\//s/.* \([a-z0-9_]*\)(.*/\1/p' \
	    $(PRIMS) > primitives

prims.c : primitives
	(echo '#include "mlvalues.h"'; \
	 echo '#include "prims.h"'; \
	 sed -e 's/.*/extern value &();/' primitives; \
	 echo 'c_primitive cprim[] = {'; \
	 sed -e 's/.*/	&,/' primitives; \
	 echo '	 0 };'; \
	 echo 'char * names_of_cprim[] = {'; \
	 sed -e 's/.*/	"&",/' primitives; \
	 echo '	 0 };') > prims.c

opnames.h : instruct.h
	sed -e '/\/\*/d' \
	    -e 's/enum /char * names_of_/' \
	    -e 's/{$$/[] = {/' \
	    -e 's/\([A-Z][A-Z_0-9]*\)/"\1"/g' instruct.h > opnames.h

# jumptbl.h is required only if you have GCC 2.0 or later
jumptbl.h : instruct.h
	sed -n -e '/^  /s/ \([A-Z]\)/ \&\&lbl_\1/gp' \
	       -e '/^}/q' instruct.h > jumptbl.h

.SUFFIXES: .d.o

.c.d.o:
	@ if test -f $*.o; then mv $*.o $*.f.o; else :; fi
	$(CC) -c $(DFLAGS) $<
	mv $*.o $*.d.o
	@ if test -f $*.f.o; then mv $*.f.o $*.o; else :; fi

depend : prims.c opnames.h jumptbl.h
	gcc -MM *.c > .depend
	gcc -MM -DDEBUG *.c | sed -e 's/\.o/.d.o/' >> .depend

include .depend
