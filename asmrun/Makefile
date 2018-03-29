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
include ../Makefile.common

LINKEDFILES=misc.c freelist.c major_gc.c minor_gc.c memory.c alloc.c array.c \
  compare.c ints.c floats.c str.c io.c extern.c intern.c hash.c sys.c \
  parsing.c gc_ctrl.c md5.c obj.c lexing.c printexc.c callback.c \
  weak.c compact.c finalise.c meta.c custom.c main.c globroots.c \
  $(UNIX_OR_WIN32).c dynlink.c signals.c debugger.c startup_aux.c \
  backtrace.c afl.c bigarray.c

# The following variable stores the list of files for which dependencies
# should be computed. It includes even the files that won't actually be
# compiled on the platform where make depend is run
sources := $(LINKEDFILES)

ifeq "$(UNIX_OR_WIN32)" "win32"
LN = cp
sources += ../byterun/unix.c
else
LN = ln -s
sources += ../byterun/win32.c
endif

CPPFLAGS += -I../byterun -DNATIVE_CODE -DTARGET_$(ARCH)

ifeq "$(UNIX_OR_WIN32)" "unix"
CPPFLAGS += -DMODEL_$(MODEL)
endif

CPPFLAGS += -DSYS_$(SYSTEM) $(IFLEXDIR) $(LIBUNWIND_INCLUDE_FLAGS)

ifneq "$(CCOMPTYPE)" "msvc"
CFLAGS += -g
endif

ifeq "$(TOOLCHAIN)" "msvc"
DFLAGS = $(CFLAGS) -DDEBUG
PFLAGS=$(CFLAGS) -DPROFILING $(NATIVECCPROFOPTS)
ASMOBJS=$(ARCH)nt.$(O)
ASMFLAGS=
ifeq ($(WITH_SPACETIME),true)
ASMFLAGS=/DWITH_SPACETIME
endif
else
DFLAGS = $(CFLAGS) -g -DDEBUG
PFLAGS=$(CFLAGS) -pg -DPROFILING $(NATIVECCPROFOPTS)
ASMOBJS=$(ARCH).$(O)
endif

IFLAGS=$(CFLAGS) -DCAML_INSTR
PICFLAGS=$(CFLAGS) $(SHAREDCCCOMPOPTS)

ASPPFLAGS = -DSYS_$(SYSTEM) -I../byterun
ifeq "$(UNIX_OR_WIN32)" "unix"
ASPPFLAGS += -DMODEL_$(MODEL)
endif

COBJS=startup_aux.$(O) startup.$(O) main.$(O) fail.$(O)		\
  roots.$(O) signals.$(O) signals_asm.$(O) misc.$(O) freelist.$(O)	\
  major_gc.$(O) minor_gc.$(O) memory.$(O) alloc.$(O) compare.$(O)	\
  ints.$(O) floats.$(O) str.$(O) array.$(O) io.$(O) extern.$(O)		\
  intern.$(O) hash.$(O) sys.$(O) parsing.$(O) gc_ctrl.$(O)		\
  md5.$(O) obj.$(O) lexing.$(O) $(UNIX_OR_WIN32).$(O)	\
  printexc.$(O) callback.$(O) weak.$(O) compact.$(O) finalise.$(O)	\
  custom.$(O) globroots.$(O) backtrace_prim.$(O) backtrace.$(O)		\
  natdynlink.$(O) debugger.$(O) meta.$(O) dynlink.$(O)			\
  clambda_checks.$(O) spacetime.$(O) spacetime_snapshot.$(O)		\
  afl.$(O) bigarray.$(O)

OBJS=$(COBJS) $(ASMOBJS)

DOBJS=$(COBJS:.$(O)=.d.$(O)) $(ASMOBJS)
IOBJS=$(COBJS:.$(O)=.i.$(O)) $(ASMOBJS)
POBJS=$(COBJS:.$(O)=.p.$(O)) $(ASMOBJS:.$(O)=.p.$(O))
PICOBJS=$(COBJS:.$(O)=.pic.$(O)) $(ASMOBJS:.$(O)=.pic.$(O))

TARGETS_A = libasmrun.$(A)
TARGETS_SO=

ifeq "$(RUNTIMED)" "true"
TARGETS_A += libasmrund.$(A)
endif

ifeq "$(RUNTIMEI)" "true"
TARGETS_A += libasmruni.$(A)
endif

ifeq "$(PROFILING)" "true"
TARGETS_A += libasmrunp.$(A)
endif

ifeq "$(UNIX_OR_WIN32)" "unix"
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
TARGETS_A += libasmrun_pic.$(A)
TARGETS_SO += libasmrun_shared.$(SO)
endif
endif

.PHONY: all
all: $(TARGETS_A) $(TARGETS_SO)

libasmrun.$(A): $(OBJS)
	$(call MKLIB,$@, $^)

libasmrund.$(A): $(DOBJS)
	$(call MKLIB,$@, $^)

libasmruni.$(A): $(IOBJS)
	$(call MKLIB,$@, $^)

libasmrunp.$(A): $(POBJS)
	$(call MKLIB,$@, $^)

libasmrun_pic.$(A): $(PICOBJS)
	$(call MKLIB,$@, $^)

libasmrun_shared.$(SO): $(PICOBJS)
	$(MKDLL) -o $@ $^ $(NATIVECCLIBS)

.PHONY: install
install:
	$(INSTALL_DATA) $(TARGETS_A) "$(INSTALL_LIBDIR)"
	if test -n "$(TARGETS_SO)"; then \
	  $(INSTALL_PROG) $(TARGETS_SO) "$(INSTALL_LIBDIR)"; \
	fi

$(LINKEDFILES): %.c: ../byterun/%.c
	$(LN) $< $@

%.d.$(O): %.c
	$(CC) -c $(DFLAGS) $(CPPFLAGS) $(OUTPUTOBJ)$@ $<

%.i.$(O): %.c
	$(CC) -c $(IFLAGS) $(CPPFLAGS) $(OUTPUTOBJ)$@ $<

%.p.$(O): %.c
	$(CC) -c $(PFLAGS) $(CPPFLAGS) $(OUTPUTOBJ)$@ $<

%.pic.$(O): %.c
	$(CC) -c $(PICFLAGS) $(CPPFLAGS) $(OUTPUTOBJ)$@ $<

%.$(O): %.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $<

%.o: %.S
	$(ASPP) $(ASPPFLAGS) -o $@ $< || \
	{ echo "If your assembler produced syntax errors, it is probably";\
          echo "unhappy with the preprocessor. Check your assembler, or";\
          echo "try producing $*.o by hand.";\
          exit 2; }

%.p.o: %.S
	$(ASPP) $(ASPPFLAGS) $(ASPPPROFFLAGS) -o $@ $<

%.pic.o: %.S
	$(ASPP) $(ASPPFLAGS) $(SHAREDCCCOMPOPTS) -o $@ $<

%.obj: %.asm
	$(ASM)$@ $(ASMFLAGS) $<

%.pic.obj: %.asm
	$(ASM)$@ $(ASMFLAGS) $<

.PHONY: clean
clean:
	rm -f $(LINKEDFILES)
	rm -f *.$(O) *.$(A) *.$(SO)

.PHONY: distclean
distclean: clean
	rm -r *~

.PHONY: depend
ifeq "$(TOOLCHAIN)" "msvc"
depend:
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
depend: $(COBJS:.$(O)=.c) $(sources)
	$(CC) -MM $(CFLAGS) $(CPPFLAGS) $^ | sed -e 's/\.o/.$$(O)/' > .depend
	$(CC) -MM $(PFLAGS) $(CPPFLAGS) $^ | sed -e 's/\.o/.p.$$(O)/' \
	  >> .depend
	$(CC) -MM $(DFLAGS) $(CPPFLAGS) $^ | sed -e 's/\.o/.d.$$(O)/' \
	  >> .depend
	$(CC) -MM $(IFLAGS) $(CPPFLAGS) $^ | sed -e 's/\.o/.i.$$(O)/' \
	  >> .depend
endif

include .depend
