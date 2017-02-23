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

LINKEDFILES=misc.c freelist.c major_gc.c minor_gc.c memory.c alloc.c array.c \
  compare.c ints.c floats.c str.c io.c extern.c intern.c hash.c sys.c \
  parsing.c gc_ctrl.c terminfo.c md5.c obj.c lexing.c printexc.c callback.c \
  weak.c compact.c finalise.c meta.c custom.c main.c globroots.c \
  $(UNIX_OR_WIN32).c dynlink.c signals.c debugger.c startup_aux.c \
  backtrace.c afl.c

INSTALL_LIBDIR=$(DESTDIR)$(LIBDIR)

CC=$(NATIVECC)

ifeq "$(UNIX_OR_WIN32)" "win32"
LN = cp
else
LN = ln -s
endif

FLAGS=\
  -I../byterun \
  -DNATIVE_CODE -DTARGET_$(ARCH)

ifeq "$(UNIX_OR_WIN32)" "unix"
FLAGS += -DMODEL_$(MODEL)
endif

FLAGS += -DSYS_$(SYSTEM) \
  $(NATIVECCCOMPOPTS) $(IFLEXDIR) \
  $(LIBUNWIND_INCLUDE_FLAGS)

ifeq "$(TOOLCHAIN)" "msvc"
DFLAGS=$(FLAGS) -DDEBUG
PFLAGS=$(FLAGS) -DPROFILING $(NATIVECCPROFOPTS)
OUTPUTOBJ = -Fo
ASMOBJS=$(ARCH)nt.$(O)
else
DFLAGS=$(FLAGS) -g -DDEBUG
PFLAGS=$(FLAGS) -pg -DPROFILING $(NATIVECCPROFOPTS)
OUTPUTOBJ = -o
ASMOBJS=$(ARCH).$(O)
endif

IFLAGS=$(FLAGS) -DCAML_INSTR
PICFLAGS=$(FLAGS) $(SHAREDCCCOMPOPTS)

ASPPFLAGS = -DSYS_$(SYSTEM)
ifeq "$(UNIX_OR_WIN32)" "unix"
ASPPFLAGS += -DMODEL_$(MODEL)
CFLAGS=$(FLAGS) -g
else
CFLAGS=$(FLAGS)
endif

COBJS=startup_aux.$(O) startup.$(O) main.$(O) fail.$(O)		\
  roots.$(O) signals.$(O) signals_asm.$(O) misc.$(O) freelist.$(O)	\
  major_gc.$(O) minor_gc.$(O) memory.$(O) alloc.$(O) compare.$(O)	\
  ints.$(O) floats.$(O) str.$(O) array.$(O) io.$(O) extern.$(O)		\
  intern.$(O) hash.$(O) sys.$(O) parsing.$(O) gc_ctrl.$(O)		\
  terminfo.$(O) md5.$(O) obj.$(O) lexing.$(O) $(UNIX_OR_WIN32).$(O)	\
  printexc.$(O) callback.$(O) weak.$(O) compact.$(O) finalise.$(O)	\
  custom.$(O) globroots.$(O) backtrace_prim.$(O) backtrace.$(O)		\
  natdynlink.$(O) debugger.$(O) meta.$(O) dynlink.$(O)			\
  clambda_checks.$(O) spacetime.$(O) spacetime_snapshot.$(O)		\
  spacetime_offline.$(O) afl.$(O)

OBJS=$(COBJS) $(ASMOBJS)

DOBJS=$(COBJS:.$(O)=.d.$(O)) $(ASMOBJS)
IOBJS=$(COBJS:.$(O)=.i.$(O)) $(ASMOBJS)
POBJS=$(COBJS:.$(O)=.p.$(O)) $(ASMOBJS:.$(O)=.p.$(O))
PICOBJS=$(COBJS:.$(O)=.pic.$(O)) $(ASMOBJS:.$(O)=.pic.$(O))

TARGETS = libasmrun.$(A)

ifeq "$(RUNTIMED)" "true"
TARGETS += libasmrund.$(A)
endif

ifeq "$(RUNTIMEI)" "true"
TARGETS += libasmruni.$(A)
endif

ifeq "$(PROFILING)" "true"
TARGETS += libasmrunp.$(A)
endif

ifeq "$(UNIX_OR_WIN32)" "unix"
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
TARGETS += libasmrun_pic.$(A) libasmrun_shared.$(SO)
endif
endif

.PHONY: all
all: $(TARGETS)

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
	cp $(TARGETS) "$(INSTALL_LIBDIR)"

$(LINKEDFILES): %.c: ../byterun/%.c
	$(LN) $< $@

%.d.$(O): %.c
	$(CC) -c $(DFLAGS) $(OUTPUTOBJ)$@ $<

%.i.$(O): %.c
	$(CC) -c $(IFLAGS) $(OUTPUTOBJ)$@ $<

%.p.$(O): %.c
	$(CC) -c $(PFLAGS) $(OUTPUTOBJ)$@ $<

%.pic.$(O): %.c
	$(CC) -c $(PICFLAGS) $(OUTPUTOBJ)$@ $<

%.$(O): %.c
	$(CC) $(CFLAGS) -c $<

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
	$(ASM)$@ $<

%.pic.obj: %.asm
	$(ASM)$@ $<

.PHONY: clean
clean:
	rm -f $(LINKEDFILES)
	rm -f *.$(O) *.$(A) *.$(SO)

.PHONY: distclean
distclean: clean
	rm -r *~

ifneq "$(TOOLCHAIN)" "msvc"
.PHONY: depend
depend: $(COBJS:.$(O)=.c) $(LINKEDFILES)
	$(CC) -MM $(FLAGS) *.c > .depend
	$(CC) -MM $(FLAGS) -DPROFILING *.c | sed -e 's/\.o/.p.o/' >> .depend
	$(CC) -MM $(FLAGS) -DDEBUG *.c | sed -e 's/\.o/.d.o/' >> .depend
	$(CC) -MM $(FLAGS) -DCAML_INSTR *.c | sed -e 's/\.o/.i.o/' >> .depend
endif

ifeq "$(UNIX_OR_WIN32)" "win32"
.depend.nt: .depend
	sed -e 's/\.o/.$(O)/g' .depend > .depend.nt

include .depend.nt

else
include .depend
endif
