include ../config/Makefile.h
include ../Makefile.config

CFLAGS=-O $(CCCOMPOPTS)
DFLAGS=-g -DDEBUG $(CCCOMPOPTS)

OBJS=interp.o misc.o stacks.o fix_code.o main.o fail.o signals.o \
  freelist.o major_gc.o minor_gc.o memory.o alloc.o roots.o \
  compare.o ints.o floats.o str.o array.o io.o extern.o intern.o \
  hash.o sys.o meta.o parsing.o lexing.o gc_ctrl.o terminfo.o crc.o

DOBJS=$(OBJS:.o=.d.o) instrtrace.d.o

PRIMS=array.c compare.c crc.c extern.c floats.c gc_ctrl.c hash.c \
  intern.c interp.c ints.c io.c lexing.c meta.c parsing.c \
  signals.c str.c sys.c terminfo.c

all: camlrun camlrund

camlrun: $(OBJS) prims.o
	$(CC) $(CCCOMPOPTS) $(CCLINKOPTS) $(LOWADDRESSES) -o camlrun prims.o $(OBJS) $(CCLIBS)

camlrund: $(DOBJS) prims.o
	$(CC) -g $(CCCOMPOPTS) $(CCLINKOPTS) $(LOWADDRESSES) -o camlrund prims.o $(DOBJS) $(CCLIBS)

install:
	cp camlrun $(BINDIR)/cslrun
	ar rc $(LIBDIR)/libcamlrun.a $(OBJS)
	$(RANLIB) $(LIBDIR)/libcamlrun.a
	test -d $(LIBDIR)/caml || mkdir $(LIBDIR)/caml
	cp mlvalues.h alloc.h misc.h $(LIBDIR)/caml
	sed -e '/#include ".*\/m.h/r ../config/m.h' \
            -e '/#include ".*\/s.h/r ../config/s.h' \
            -e '/#include "/d' config.h > $(LIBDIR)/caml/config.h
	sed -e '/#include ".*gc\.h"/d' \
	    -e '/#define Alloc_small/,/^}/d' \
	    -e '/Modify/,/^}/d' memory.h > $(LIBDIR)/caml/memory.h

clean:
	rm -f camlrun camlrund *.o *.a
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
	cd .debugobj; $(CC) -c $(DFLAGS) -I.. ../$<
	mv .debugobj/$*.o $*.d.o

depend : prims.c opnames.h jumptbl.h
	gcc -MM $(CFLAGS) *.c > .depend
	gcc -MM $(DFLAGS) *.c | sed -e 's/\.o/.d.o/' >> .depend

include .depend
