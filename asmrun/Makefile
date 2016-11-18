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

include ../config/Makefile

include Makefile.shared

CC=$(NATIVECC)
FLAGS=-I../byterun -DNATIVE_CODE \
      -DTARGET_$(ARCH) -DMODEL_$(MODEL) -DSYS_$(SYSTEM) $(IFLEXDIR) \
      $(LIBUNWIND_INCLUDE_FLAGS)
#CFLAGS=$(FLAGS) -g -O0
CFLAGS=$(FLAGS) -g -O0 $(NATIVECCCOMPOPTS)
DFLAGS=$(FLAGS) -g -DDEBUG $(NATIVECCCOMPOPTS)
IFLAGS=$(FLAGS) -DCAML_INSTR
PFLAGS=$(FLAGS) -pg -DPROFILING $(NATIVECCPROFOPTS) $(NATIVECCCOMPOPTS)
PICFLAGS=$(FLAGS) $(SHAREDCCCOMPOPTS) $(NATIVECCCOMPOPTS)

ASMOBJS=$(ARCH).o

OBJS=$(COBJS) $(ASMOBJS)
DOBJS=$(COBJS:.o=.d.o) $(ASMOBJS)
IOBJS=$(COBJS:.o=.i.o) $(ASMOBJS)
POBJS=$(COBJS:.o=.p.o) $(ASMOBJS:.o=.p.o)
PICOBJS=$(COBJS:.o=.pic.o) $(ASMOBJS:.o=.pic.o)

all: libasmrun.a all-$(RUNTIMED) all-$(PROFILING) all-$(SHARED)

ifeq "$(RUNTIMEI)" "true"
all: libasmruni.a
endif

libasmrun.a: $(OBJS)
	rm -f libasmrun.a
	$(ARCMD) rc libasmrun.a $(OBJS)
	$(RANLIB) libasmrun.a

all-noruntimed:
.PHONY: all-noruntimed

all-runtimed: libasmrund.a
.PHONY: all-runtimed

libasmrund.a: $(DOBJS)
	rm -f libasmrund.a
	$(ARCMD) rc libasmrund.a $(DOBJS)
	$(RANLIB) libasmrund.a

libasmruni.a: $(IOBJS)
	rm -f $@
	$(ARCMD) rc $@ $^
	$(RANLIB) $@

all-noprof:

all-prof: libasmrunp.a

libasmrunp.a: $(POBJS)
	rm -f libasmrunp.a
	$(ARCMD) rc libasmrunp.a $(POBJS)
	$(RANLIB) libasmrunp.a

all-noshared:

all-shared: libasmrun_pic.a libasmrun_shared.so

libasmrun_pic.a: $(PICOBJS)
	rm -f libasmrun_pic.a
	$(ARCMD) rc libasmrun_pic.a $(PICOBJS)
	$(RANLIB) libasmrun_pic.a

libasmrun_shared.so: $(PICOBJS)
	$(MKDLL) -o libasmrun_shared.so $(PICOBJS) $(NATIVECCLIBS)

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)

install::
	cp libasmrun.a $(INSTALL_LIBDIR)/libasmrun.a
	cd $(INSTALL_LIBDIR); $(RANLIB) libasmrun.a
.PHONY: install-default

ifeq "$(RUNTIMED)" "runtimed"
install::
	cp libasmrund.a $(INSTALL_LIBDIR)/libasmrund.a
	cd $(INSTALL_LIBDIR); $(RANLIB) libasmrund.a
endif

ifeq "$(RUNTIMEI)" "true"
install::
	cp libasmruni.a $(INSTALL_LIBDIR)/libasmruni.a
	cd $(INSTALL_LIBDIR); $(RANLIB) libasmruni.a
endif

ifeq "$(PROFILING)" "prof"
install::
	cp libasmrunp.a $(INSTALL_LIBDIR)/libasmrunp.a
	cd $(INSTALL_LIBDIR); $(RANLIB) libasmrunp.a
else
install::
	rm -f $(INSTALL_LIBDIR)/libasmrunp.a
	ln -s libasmrun.a $(INSTALL_LIBDIR)/libasmrunp.a
endif

ifeq "$(SHARED)" "shared"
install::
	cp libasmrun_pic.a $(INSTALL_LIBDIR)/libasmrun_pic.a
	cd $(INSTALL_LIBDIR); $(RANLIB) libasmrunp.a
	cp libasmrun_shared.so $(INSTALL_LIBDIR)/libasmrun_shared.so
endif

$(LINKEDFILES): %.c: ../byterun/%.c
	ln -s ../byterun/$*.c $*.c

clean::
	rm -f $(LINKEDFILES)

%.d.o: %.c
	$(CC) -c $(DFLAGS) -o $@ $<

%.i.o : %.c
	$(CC) -c $(IFLAGS) -o $@ $<

%.p.o: %.c
	$(CC) -c $(PFLAGS) -o $@ $<

%.pic.o: %.c
	$(CC) -c $(PICFLAGS) -o $@ $<

%.o: %.S
	$(ASPP) -DSYS_$(SYSTEM) -DMODEL_$(MODEL) -o $@ $< || \
	{ echo "If your assembler produced syntax errors, it is probably";\
          echo "unhappy with the preprocessor. Check your assembler, or";\
          echo "try producing $*.o by hand.";\
          exit 2; }

%.p.o: %.S
	$(ASPP) -DSYS_$(SYSTEM) -DMODEL_$(MODEL) $(ASPPPROFFLAGS) -o $@ $<

%.pic.o: %.S
	$(ASPP) -DSYS_$(SYSTEM) -DMODEL_$(MODEL) $(SHAREDCCCOMPOPTS) -o $@ $<

%.o: %.s
	$(ASPP) -DSYS_$(SYSTEM) -o $@ $<

%.p.o: %.s
	$(ASPP) -DSYS_$(SYSTEM) $(ASPPPROFFLAGS) -o $@ $<

%.pic.o: %.s
	$(ASPP) -DSYS_$(SYSTEM) $(SHAREDCCCOMPOPTS) -o $@ $<

clean::
	rm -f *.o *.a *.so *~

depend: $(COBJS:.o=.c) ${LINKEDFILES}
	$(CC) -MM $(FLAGS) *.c > .depend
	$(CC) -MM $(FLAGS) -DPROFILING *.c | sed -e 's/\.o/.p.o/' >> .depend
	$(CC) -MM $(FLAGS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' >> .depend
	$(CC) -MM $(FLAGS) -DCAML_INSTR *.c | sed -e 's/\.o/.i.o/' >> .depend

include .depend
