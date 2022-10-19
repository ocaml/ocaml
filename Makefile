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

# The main Makefile

ROOTDIR = .
# NOTE: it is important that the OCAMLDEP and OCAMLLEX variables
# are defined *before* Makefile.common gets included, so that
# their local definitions here take precedence over their
# general shared definitions in Makefile.common.
OCAMLDEP ?= $(BOOT_OCAMLDEP)
OCAMLLEX ?= $(BOOT_OCAMLLEX)
include Makefile.common

.PHONY: defaultentry
defaultentry: $(DEFAULT_BUILD_TARGET)

ifeq "$(UNIX_OR_WIN32)" "win32"
LN = cp
else
LN = ln -sf
endif

include stdlib/StdlibModules

CAMLC = $(BOOT_OCAMLC) $(BOOT_STDLIBFLAGS) -use-prims runtime/primitives
CAMLOPT=$(OCAMLRUN) ./ocamlopt$(EXE) $(STDLIBFLAGS) -I otherlibs/dynlink
ARCHES=amd64 i386 arm arm64 power s390x riscv
DIRS = utils parsing typing bytecomp file_formats lambda middle_end \
  middle_end/closure middle_end/flambda middle_end/flambda/base_types \
  asmcomp driver toplevel
INCLUDES = $(addprefix -I ,$(DIRS))

ifeq "$(strip $(NATDYNLINKOPTS))" ""
OCAML_NATDYNLINKOPTS=
else
OCAML_NATDYNLINKOPTS = -ccopt "$(NATDYNLINKOPTS)"
endif

OC_OCAMLDEPDIRS = $(DIRS)

OCAMLDOC_OPT=$(WITH_OCAMLDOC:=.opt)
OCAMLTEST_OPT=$(WITH_OCAMLTEST:=.opt)

BYTESTART=driver/main.cmo

OPTSTART=driver/optmain.cmo

TOPLEVELSTART=toplevel/topstart.cmo

TOPLEVELINIT=toplevel/toploop.cmo

# This list is passed to expunge, which accepts both uncapitalized and
# capitalized module names.
PERVASIVES=$(STDLIB_MODULES) outcometree topprinters topdirs toploop

LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

COMPLIBDIR=$(LIBDIR)/compiler-libs

TOPINCLUDES=$(addprefix -I otherlibs/,$(filter-out %threads,$(OTHERLIBRARIES)))

ifeq "$(BOOTSTRAPPING_FLEXDLL)" "false"
  COLDSTART_DEPS =
else
  COLDSTART_DEPS = boot/ocamlruns$(EXE)
endif

expunge := expunge$(EXE)

# targets for the compilerlibs/*.{cma,cmxa} archives
include compilerlibs/Makefile.compilerlibs

# The configuration file

utils/config.ml: \
  utils/config_$(if $(filter true,$(IN_COREBOOT_CYCLE)),boot,main).ml
	cp $< $@
utils/config_boot.ml: utils/config.fixed.ml utils/config.common.ml
	cat $^ > $@

utils/config_main.ml: utils/config.generated.ml utils/config.common.ml
	cat $^ > $@

.PHONY: reconfigure
reconfigure:
	ac_read_git_config=true ./configure $(CONFIGURE_ARGS)

utils/domainstate.ml: utils/domainstate.ml.c runtime/caml/domain_state.tbl
	$(CPP) -I runtime/caml $< > $@

utils/domainstate.mli: utils/domainstate.mli.c runtime/caml/domain_state.tbl
	$(CPP) -I runtime/caml $< > $@

configure: configure.ac aclocal.m4 build-aux/ocaml_version.m4 tools/autogen
	tools/autogen

.PHONY: partialclean
partialclean::
	rm -f utils/config.ml \
	      utils/config_main.ml utils/config_main.mli \
	      utils/config_boot.ml utils/config_boot.mli \
        utils/domainstate.ml utils/domainstate.mli

.PHONY: beforedepend
beforedepend:: \
  utils/config.ml utils/config_boot.ml utils/config_main.ml \
  utils/domainstate.ml utils/domainstate.mli

ocamllex_PROGRAMS := $(addprefix lex/,ocamllex ocamllex.opt)

ocamlyacc_PROGRAM = yacc/ocamlyacc

PROGRAMS = expunge ocaml ocamlc ocamlc.opt ocamlnat ocamlopt ocamlopt.opt \
  $(ocamllex_PROGRAMS) $(ocamlyacc_PROGRAM)

$(foreach PROGRAM, $(PROGRAMS), $(eval $(call PROGRAM_SYNONYM,$(PROGRAM))))

USE_RUNTIME_PRIMS = -use-prims ../runtime/primitives
USE_STDLIB = -nostdlib -I ../stdlib

FLEXDLL_OBJECTS = \
  flexdll_$(FLEXDLL_CHAIN).$(O) flexdll_initer_$(FLEXDLL_CHAIN).$(O)
FLEXLINK_BUILD_ENV = \
  MSVC_DETECT=0 OCAML_CONFIG_FILE=../Makefile.config \
  CHAINS=$(FLEXDLL_CHAIN) ROOTDIR=..
FLEXDLL_SOURCE_FILES = \
  $(wildcard $(FLEXDLL_SOURCES)/*.c) $(wildcard $(FLEXDLL_SOURCES)/*.h) \
  $(wildcard $(FLEXDLL_SOURCES)/*.ml)

boot/ocamlruns$(EXE): runtime/ocamlruns$(EXE)
	cp $< $@

boot/flexlink.byte$(EXE): $(FLEXDLL_SOURCE_FILES)
	$(MAKE) -C $(FLEXDLL_SOURCES) $(FLEXLINK_BUILD_ENV) \
	  OCAMLRUN='$$(ROOTDIR)/boot/ocamlruns$(EXE)' NATDYNLINK=false \
	  OCAMLOPT='$(value BOOT_OCAMLC) $(USE_RUNTIME_PRIMS) $(USE_STDLIB)' \
	  -B flexlink.exe support
	cp $(FLEXDLL_SOURCES)/flexlink.exe boot/flexlink.byte$(EXE)
	cp $(addprefix $(FLEXDLL_SOURCES)/, $(FLEXDLL_OBJECTS)) boot/

# Start up the system from the distribution compiler
# The process depends on whether FlexDLL is also being bootstrapped.
# Normal procedure:
#   - Build the runtime
#   - Build the standard library using runtime/ocamlrun
# FlexDLL procedure:
#   - Build ocamlruns
#   - Build the standard library using boot/ocamlruns
#   - Build flexlink and FlexDLL support objects
#   - Build the runtime
# runtime/ocamlrun is then installed to boot/ocamlrun and the stdlib artefacts
# are copied to boot/
.PHONY: coldstart
coldstart: $(COLDSTART_DEPS)
ifeq "$(BOOTSTRAPPING_FLEXDLL)" "false"
	$(MAKE) runtime-all
	$(MAKE) -C stdlib \
	  OCAMLRUN='$$(ROOTDIR)/runtime/ocamlrun$(EXE)' \
	  CAMLC='$$(BOOT_OCAMLC) $(USE_RUNTIME_PRIMS)' all
else
	$(MAKE) -C stdlib OCAMLRUN='$$(ROOTDIR)/boot/ocamlruns$(EXE)' \
    CAMLC='$$(BOOT_OCAMLC)' all
	$(MAKE) boot/flexlink.byte$(EXE)
	$(MAKE) runtime-all
endif # ifeq "$(BOOTSTRAPPING_FLEXDLL)" "false"
	rm -f boot/ocamlrun$(EXE)
	cp runtime/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	cd boot; rm -f $(LIBFILES)
	cd stdlib; cp $(LIBFILES) ../boot
	cd boot; $(LN) ../runtime/libcamlrun.$(A) .

# Recompile the core system using the bootstrap compiler
.PHONY: coreall
coreall: runtime
	$(MAKE) ocamlc
	$(MAKE) ocamllex ocamltools library

# Build the core system: the minimum needed to make depend and bootstrap
.PHONY: core
core: coldstart
	$(MAKE) coreall

# Check if fixpoint reached

# We use tools/cmpbyt because it has better error reporting, but cmp could also
# be used.
CMPCMD ?= $(OCAMLRUN) tools/cmpbyt$(EXE)

.PHONY: compare
compare:
# The core system has to be rebuilt after bootstrap anyway, so strip ocamlc
# and ocamllex, which means the artefacts should be identical.
	mv ocamlc$(EXE) ocamlc.tmp
	$(OCAMLRUN) tools/stripdebug ocamlc.tmp ocamlc$(EXE)
	mv lex/ocamllex$(EXE) ocamllex.tmp
	$(OCAMLRUN) tools/stripdebug ocamllex.tmp lex/ocamllex$(EXE)
	rm -f ocamllex.tmp ocamlc.tmp
	@if $(CMPCMD) boot/ocamlc ocamlc$(EXE) \
         && $(CMPCMD) boot/ocamllex lex/ocamllex$(EXE); \
	then echo "Fixpoint reached, bootstrap succeeded."; \
	else \
	  echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	  exit 1; \
	fi

# Promote a compiler

PROMOTE ?= cp

.PHONY: promote-common
promote-common:
	$(PROMOTE) ocamlc$(EXE) boot/ocamlc
	$(PROMOTE) lex/ocamllex$(EXE) boot/ocamllex
	cd stdlib; cp $(LIBFILES) ../boot

# Promote the newly compiled system to the rank of cross compiler
# (Runs on the old runtime, produces code for the new runtime)
.PHONY: promote-cross
promote-cross: promote-common

# Promote the newly compiled system to the rank of bootstrap compiler
# (Runs on the new runtime, produces code for the new runtime)
.PHONY: promote
promote: PROMOTE = $(OCAMLRUN) tools/stripdebug
promote: promote-common
	rm -f boot/ocamlrun$(EXE)
	cp runtime/ocamlrun$(EXE) boot/ocamlrun$(EXE)

# Compile the native-code compiler
.PHONY: opt-core
opt-core: runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) libraryopt

.PHONY: opt
opt: checknative
	$(MAKE) runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) libraryopt
	$(MAKE) otherlibrariesopt ocamltoolsopt

# Native-code versions of the tools
.PHONY: opt.opt
opt.opt: checknative
	$(MAKE) checkstack
	$(MAKE) coreall
	$(MAKE) ocaml
	$(MAKE) opt-core
ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
	$(MAKE) flexlink.opt$(EXE)
endif
	$(MAKE) ocamlc.opt
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) \
	  $(WITH_OCAMLTEST)
	$(MAKE) ocamlopt.opt
	$(MAKE) otherlibrariesopt
	$(MAKE) ocamllex.opt ocamltoolsopt ocamltoolsopt.opt $(OCAMLDOC_OPT) \
	  $(OCAMLTEST_OPT) othertools ocamlnat
ifeq "$(WITH_OCAMLDOC)-$(STDLIB_MANPAGES)" "ocamldoc-true"
	$(MAKE) manpages
endif

# Core bootstrapping cycle
.PHONY: coreboot
ifeq "$(FLAT_FLOAT_ARRAY)" "true"
coreboot:
# Promote the new compiler but keep the old runtime
# This compiler runs on boot/ocamlrun and produces bytecode for
# runtime/ocamlrun
	$(MAKE) promote-cross
# Rebuild ocamlc and ocamllex (run on runtime/ocamlrun)
# utils/config.ml will have the fixed bootstrap configuration
	$(MAKE) partialclean
	$(MAKE) IN_COREBOOT_CYCLE=true ocamlc ocamllex ocamltools
# Rebuild the library (using runtime/ocamlrun ./ocamlc)
	$(MAKE) library-cross
# Promote the new compiler and the new runtime
	$(MAKE) OCAMLRUN=runtime/ocamlrun$(EXE) promote
# Rebuild the core system
# utils/config.ml must still have the fixed bootstrap configuration
	$(MAKE) partialclean
	$(MAKE) IN_COREBOOT_CYCLE=true core
# Check if fixpoint reached
	$(MAKE) compare
else
coreboot:
	$(error Cannot bootstrap when configured with \
--disable-flat-float-array)
endif

# Recompile the system using the bootstrap compiler

.PHONY: all
all: coreall
	$(MAKE) ocaml
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) \
         $(WITH_OCAMLTEST)
	$(MAKE) othertools
ifeq "$(WITH_OCAMLDOC)-$(STDLIB_MANPAGES)" "ocamldoc-true"
	$(MAKE) manpages
endif

# Bootstrap and rebuild the whole system.
# The compilation of ocaml will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.
.PHONY: bootstrap
bootstrap: coreboot
# utils/config.ml must be restored to config.status's configuration
# lex/ocamllex$(EXE) was stripped in order to compare it
	rm -f utils/config.ml lex/ocamllex$(EXE)
	$(MAKE) all

# Compile everything the first time

.PHONY: world
world: coldstart
	$(MAKE) all

# Compile also native code compiler and libraries, fast
.PHONY: world.opt
world.opt: checknative
	$(MAKE) coldstart
	$(MAKE) opt.opt

# FlexDLL sources missing error messages
# Different git mechanism displayed depending on whether this source tree came
# from a git clone or a source tarball.

.PHONY: flexdll flexlink flexlink.opt

ifeq "$(BOOTSTRAPPING_FLEXDLL)" "false"
flexdll flexlink flexlink.opt:
	@echo It is no longer necessary to bootstrap FlexDLL with a separate
	@echo make invocation. Simply place the sources for FlexDLL in a
	@echo sub-directory.
	@echo This can either be done by downloading a source tarball from
	@echo \  https://github.com/alainfrisch/flexdll/releases
	@if [ -d .git ]; then \
	  echo or by checking out the flexdll submodule with; \
	  echo \  git submodule update --init; \
	else \
	  echo or by cloning the git repository; \
	  echo \  git clone https://github.com/alainfrisch/flexdll.git; \
	fi
	@echo "Then pass --with-flexdll=<dir> to configure and build as normal."
	@false

else

.PHONY: flexdll
flexdll: flexdll/Makefile
	@echo WARNING! make flexdll is no longer required
	@echo This target will be removed in a future release.

.PHONY: flexlink
flexlink:
	@echo Bootstrapping just flexlink.exe is no longer supported
	@echo Bootstrapping FlexDLL is now enabled with
	@echo ./configure --with-flexdll
	@false

ifeq "$(wildcard ocamlopt.opt$(EXE))" ""
  FLEXLINK_OCAMLOPT=../runtime/ocamlrun$(EXE) ../ocamlopt$(EXE)
else
  FLEXLINK_OCAMLOPT=../ocamlopt.opt$(EXE)
endif

flexlink.opt$(EXE): $(FLEXDLL_SOURCE_FILES)
	$(MAKE) -C $(FLEXDLL_SOURCES) $(FLEXLINK_BUILD_ENV) \
    OCAML_FLEXLINK='$(value OCAMLRUN) $$(ROOTDIR)/boot/flexlink.byte$(EXE)' \
	  OCAMLOPT="$(FLEXLINK_OCAMLOPT) -nostdlib -I ../stdlib" -B flexlink.exe
	cp $(FLEXDLL_SOURCES)/flexlink.exe $@

partialclean::
	rm -f flexlink.opt$(EXE)
endif # ifeq "$(BOOTSTRAPPING_FLEXDLL)" "false"

INSTALL_COMPLIBDIR = $(DESTDIR)$(COMPLIBDIR)
INSTALL_FLEXDLLDIR = $(INSTALL_LIBDIR)/flexdll
FLEXDLL_MANIFEST = default$(filter-out _i386,_$(ARCH)).manifest

DOC_FILES=\
  Changes \
  README.adoc \
  README.win32.adoc \
  LICENSE

# Run all tests

.PHONY: tests
tests:
	$(MAKE) -C testsuite all

# Make clean in the test suite

.PHONY: clean
clean::
	$(MAKE) -C testsuite clean

# Build the manual latex files from the etex source files
# (see manual/README.md)
.PHONY: manual-pregen
manual-pregen: opt.opt
	cd manual; $(MAKE) clean && $(MAKE) pregen-etex

clean::
	$(MAKE) -C manual clean

# The clean target
clean:: partialclean
	rm -f configure~ $(PROGRAMS) $(PROGRAMS:=.exe)

# The bytecode compiler

ocamlc$(EXE): compilerlibs/ocamlcommon.cma \
              compilerlibs/ocamlbytecomp.cma $(BYTESTART)
	$(CAMLC) $(OC_COMMON_LDFLAGS) -compat-32 -o $@ $^

partialclean::
	rm -rf ocamlc$(EXE)

# The native-code compiler

ocamlopt$(EXE): compilerlibs/ocamlcommon.cma compilerlibs/ocamloptcomp.cma \
          $(OPTSTART)
	$(CAMLC) $(OC_COMMON_LDFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt$(EXE)

# The toplevel

ocaml_dependencies := \
  compilerlibs/ocamlcommon.cma \
  compilerlibs/ocamlbytecomp.cma \
  compilerlibs/ocamltoplevel.cma $(TOPLEVELSTART)

.INTERMEDIATE: ocaml.tmp
ocaml.tmp: $(ocaml_dependencies)
	$(CAMLC) $(OC_COMMON_LDFLAGS) -I toplevel/byte -linkall -o $@ $^

ocaml$(EXE): $(expunge) ocaml.tmp
	- $(OCAMLRUN) $^ $@ $(PERVASIVES)

partialclean::
	rm -f ocaml$(EXE)

# Use TOPFLAGS to pass additional flags to the bytecode or native toplevel
# when running make runtop or make natruntop
TOPFLAGS ?=
OC_TOPFLAGS = $(STDLIBFLAGS) -I toplevel -noinit $(TOPINCLUDES) $(TOPFLAGS)

# Note: Beware that, since this rule begins with a coldstart, both
# boot/ocamlrun and runtime/ocamlrun will be the same when the toplevel
# is run.
.PHONY: runtop
runtop:
	$(MAKE) coldstart
	$(MAKE) ocamlc
	$(MAKE) otherlibraries
	$(MAKE) ocaml
	@$(RLWRAP) $(OCAMLRUN) ./ocaml$(EXE) $(OC_TOPFLAGS)

.PHONY: natruntop
natruntop:
	$(MAKE) core
	$(MAKE) opt
	$(MAKE) ocamlnat
	@$(FLEXLINK_ENV) $(RLWRAP) ./ocamlnat$(EXE) $(OC_TOPFLAGS)

# Native dynlink

otherlibs/dynlink/dynlink.cmxa: otherlibs/dynlink/native/dynlink.ml
	$(MAKE) -C otherlibs/dynlink allopt

# Cleanup the lexer

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# The bytecode compiler compiled with the native-code compiler

ocamlc.opt$(EXE): compilerlibs/ocamlcommon.cmxa \
                  compilerlibs/ocamlbytecomp.cmxa $(BYTESTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(OC_COMMON_LDFLAGS) -o $@ $^ -cclib "$(BYTECCLIBS)"

partialclean::
	rm -f ocamlc.opt$(EXE)

# The native-code compiler compiled with itself

ocamlopt.opt$(EXE): \
                    compilerlibs/ocamlcommon.cmxa \
                    compilerlibs/ocamloptcomp.cmxa \
                    $(OPTSTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(OC_COMMON_LDFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt.opt$(EXE)

# The predefined exceptions and primitives

lambda/runtimedef.ml: lambda/generate_runtimedef.sh runtime/caml/fail.h \
    runtime/primitives
	$^ > $@

partialclean::
	rm -f lambda/runtimedef.ml

beforedepend:: lambda/runtimedef.ml

# Choose the right machine-dependent files

asmcomp/arch.mli: asmcomp/$(ARCH)/arch.mli
	cd asmcomp; $(LN) $(ARCH)/arch.mli .

asmcomp/arch.ml: asmcomp/$(ARCH)/arch.ml
	cd asmcomp; $(LN) $(ARCH)/arch.ml .

asmcomp/proc.ml: asmcomp/$(ARCH)/proc.ml
	cd asmcomp; $(LN) $(ARCH)/proc.ml .

asmcomp/selection.ml: asmcomp/$(ARCH)/selection.ml
	cd asmcomp; $(LN) $(ARCH)/selection.ml .

asmcomp/CSE.ml: asmcomp/$(ARCH)/CSE.ml
	cd asmcomp; $(LN) $(ARCH)/CSE.ml .

asmcomp/reload.ml: asmcomp/$(ARCH)/reload.ml
	cd asmcomp; $(LN) $(ARCH)/reload.ml .

asmcomp/scheduling.ml: asmcomp/$(ARCH)/scheduling.ml
	cd asmcomp; $(LN) $(ARCH)/scheduling.ml .

# Preprocess the code emitters

cvt_emit := tools/cvt_emit$(EXE)

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp $(cvt_emit)
	echo \# 1 \"asmcomp/$(ARCH)/emit.mlp\" > $@
	$(OCAMLRUN) $(cvt_emit) < $< >> $@ \
	|| { rm -f $@; exit 2; }

partialclean::
	rm -f asmcomp/emit.ml

beforedepend:: asmcomp/emit.ml

$(cvt_emit): tools/cvt_emit.mll
	$(MAKE) -C tools cvt_emit

# The "expunge" utility

$(expunge): compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma \
         toplevel/expunge.cmo
	$(CAMLC) $(OC_COMMON_LDFLAGS) -o $@ $^

partialclean::
	rm -f $(expunge)

# The runtime system

## Lists of source files

runtime_COMMON_C_SOURCES = \
  addrmap \
  afl \
  alloc \
  array \
  backtrace \
  bigarray \
  callback \
  codefrag \
  compare \
  custom \
  debugger \
  domain \
  dynlink \
  extern \
  fiber \
  finalise \
  floats \
  gc_ctrl \
  gc_stats \
  globroots \
  hash \
  intern \
  ints \
  io \
  lexing \
  lf_skiplist \
  main \
  major_gc \
  md5 \
  memory \
  memprof \
  meta \
  minor_gc \
  misc \
  obj \
  parsing \
  platform \
  printexc \
  prng \
  roots \
  runtime_events \
  shared_heap \
  signals \
  skiplist \
  startup_aux \
  str \
  sync \
  sys \
  $(UNIX_OR_WIN32) \
  weak

runtime_BYTECODE_ONLY_C_SOURCES = \
  backtrace_byt \
  fail_byt \
  fix_code \
  interp \
  startup_byt
runtime_BYTECODE_C_SOURCES = \
  $(runtime_COMMON_C_SOURCES:%=runtime/%.c) \
  $(runtime_BYTECODE_ONLY_C_SOURCES:%=runtime/%.c)

runtime_NATIVE_ONLY_C_SOURCES = \
  backtrace_nat \
  clambda_checks \
  dynlink_nat \
  fail_nat \
  frame_descriptors \
  startup_nat \
  signals_nat
runtime_NATIVE_C_SOURCES = \
  $(runtime_COMMON_C_SOURCES:%=runtime/%.c) \
  $(runtime_NATIVE_ONLY_C_SOURCES:%=runtime/%.c)

## Header files generated by configure
runtime_CONFIGURED_HEADERS = $(addprefix runtime/caml/, m.h s.h version.h)

## Header files generated by make
runtime_BUILT_HEADERS = $(addprefix runtime/, \
  caml/opnames.h caml/jumptbl.h build_config.h)

## Targets to build and install

runtime_PROGRAMS = runtime/ocamlrun$(EXE)
runtime_BYTECODE_STATIC_LIBRARIES = $(addprefix runtime/, \
  ld.conf libcamlrun.$(A))
runtime_BYTECODE_SHARED_LIBRARIES =
runtime_NATIVE_STATIC_LIBRARIES = runtime/libasmrun.$(A)
runtime_NATIVE_SHARED_LIBRARIES =

ifeq "$(RUNTIMED)" "true"
runtime_PROGRAMS += runtime/ocamlrund$(EXE)
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlrund.$(A)
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmrund.$(A)
endif

ifeq "$(INSTRUMENTED_RUNTIME)" "true"
runtime_PROGRAMS += runtime/ocamlruni$(EXE)
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlruni.$(A)
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmruni.$(A)
endif

ifeq "$(UNIX_OR_WIN32)" "unix"
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
runtime_BYTECODE_STATIC_LIBRARIES += runtime/libcamlrun_pic.$(A)
runtime_BYTECODE_SHARED_LIBRARIES += runtime/libcamlrun_shared.$(SO)
runtime_NATIVE_STATIC_LIBRARIES += runtime/libasmrun_pic.$(A)
runtime_NATIVE_SHARED_LIBRARIES += runtime/libasmrun_shared.$(SO)
endif
endif

## List of object files for each target

libcamlrun_OBJECTS = $(runtime_BYTECODE_C_SOURCES:.c=.b.$(O))

libcamlrun_non_shared_OBJECTS = \
  $(subst $(UNIX_OR_WIN32).b.$(O),$(UNIX_OR_WIN32)_non_shared.b.$(O), \
          $(libcamlrun_OBJECTS))

libcamlrund_OBJECTS = $(runtime_BYTECODE_C_SOURCES:.c=.bd.$(O)) \
  runtime/instrtrace.bd.$(O)

libcamlruni_OBJECTS = $(runtime_BYTECODE_C_SOURCES:.c=.bi.$(O))

libcamlrunpic_OBJECTS = $(runtime_BYTECODE_C_SOURCES:.c=.bpic.$(O))

libasmrun_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.n.$(O)) $(runtime_ASM_OBJECTS)

libasmrund_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.nd.$(O)) $(runtime_ASM_OBJECTS:.$(O)=.d.$(O))

libasmruni_OBJECTS = \
  $(runtime_NATIVE_C_SOURCES:.c=.ni.$(O)) $(runtime_ASM_OBJECTS:.$(O)=.i.$(O))

libasmrunpic_OBJECTS = $(runtime_NATIVE_C_SOURCES:.c=.npic.$(O)) \
  $(runtime_ASM_OBJECTS:.$(O)=_libasmrunpic.$(O))

## General (non target-specific) assembler and compiler flags

runtime_CPPFLAGS = -DCAMLDLLIMPORT= -DIN_CAML_RUNTIME
ocamlrund_CPPFLAGS = -DDEBUG
ocamlruni_CPPFLAGS = -DCAML_INSTR

## Runtime targets

.PHONY: runtime-all
runtime-all: \
  $(runtime_BYTECODE_STATIC_LIBRARIES) $(runtime_BYTECODE_SHARED_LIBRARIES) \
  $(runtime_PROGRAMS) $(SAK)

.PHONY: runtime-allopt
ifeq "$(NATIVE_COMPILER)" "true"
runtime-allopt: \
  $(runtime_NATIVE_STATIC_LIBRARIES) $(runtime_NATIVE_SHARED_LIBRARIES)
else
runtime-allopt:
	$(error The build has been configured with --disable-native-compiler)
endif

## Generated non-object files

runtime/ld.conf: $(ROOTDIR)/Makefile.config
	echo "$(STUBLIBDIR)" > $@
	echo "$(LIBDIR)" >> $@

# If primitives contain duplicated lines (e.g. because the code is defined
# like
# #ifdef X
# CAMLprim value caml_foo() ...
# #else
# CAMLprim value caml_foo() ...
# #endif), horrible things will happen: duplicated entries in Runtimedef ->
# double registration in Symtable -> empty entry in the PRIM table ->
# the bytecode interpreter is confused.
# We sort the primitive file and remove duplicates to avoid this problem.

# Warning: we use "sort | uniq" instead of "sort -u" because in the MSVC
# port, the "sort" program in the path is Microsoft's and not cygwin's

# Warning: POSIX sort is locale dependent, that's why we set LC_ALL explicitly.
# Sort is unstable for "is_directory" and "isatty"
# see http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sort.html:
# "using sort to process pathnames, it is recommended that LC_ALL .. set to C"

# To speed up builds, we avoid changing "primitives" when files
# containing primitives change but the primitives table does not
runtime/primitives: \
  $(shell runtime/gen_primitives.sh > runtime/primitives.new; \
                    cmp -s runtime/primitives runtime/primitives.new || \
                    echo runtime/primitives.new)
	cp $^ $@

runtime/prims.c : runtime/primitives
	(echo '#define CAML_INTERNALS'; \
         echo '#include "caml/mlvalues.h"'; \
	 echo '#include "caml/prims.h"'; \
	 sed -e 's/.*/extern value &();/' $<; \
	 echo 'c_primitive caml_builtin_cprim[] = {'; \
	 sed -e 's/.*/  &,/' $<; \
	 echo '  0 };'; \
	 echo 'char * caml_names_of_builtin_cprim[] = {'; \
	 sed -e 's/.*/  "&",/' $<; \
	 echo '  0 };') > $@

runtime/caml/opnames.h : runtime/caml/instruct.h
	tr -d '\r' < $< | \
	sed -e '/\/\*/d' \
	    -e '/^#/d' \
	    -e 's/enum /static char * names_of_/' \
	    -e 's/{$$/[] = {/' \
	    -e 's/\([[:upper:]][[:upper:]_0-9]*\)/"\1"/g' > $@

# runtime/caml/jumptbl.h is required only if you have GCC 2.0 or later
runtime/caml/jumptbl.h : runtime/caml/instruct.h
	tr -d '\r' < $< | \
	sed -n -e '/^  /s/ \([A-Z]\)/ \&\&lbl_\1/gp' \
	       -e '/^}/q' > $@

# These are provided as a temporary shim to allow cross-compilation systems
# to supply a host C compiler and different flags and a linking macro.
SAK_CC ?= $(CC)
SAK_CFLAGS ?= $(OC_CFLAGS) $(CFLAGS) $(OC_CPPFLAGS) $(CPPFLAGS)
SAK_LINK ?= $(MKEXE_VIA_CC)

$(SAK): runtime/sak.$(O)
	$(call SAK_LINK,$@,$^)

runtime/sak.$(O): runtime/sak.c runtime/caml/misc.h runtime/caml/config.h
	$(SAK_CC) -c $(SAK_CFLAGS) $(OUTPUTOBJ)$@ $<

C_LITERAL = $(shell $(SAK) encode-C-literal '$(1)')

runtime/build_config.h: $(ROOTDIR)/Makefile.config $(SAK)
	echo '/* This file is generated from $(ROOTDIR)/Makefile.config */' > $@
	echo '#define OCAML_STDLIB_DIR $(call C_LITERAL,$(LIBDIR))' >> $@
	echo '#define HOST "$(HOST)"' >> $@

## Runtime libraries and programs

runtime/ocamlrun$(EXE): runtime/prims.$(O) runtime/libcamlrun.$(A)
	$(MKEXE) -o $@ $^ $(BYTECCLIBS)

runtime/ocamlruns$(EXE): runtime/prims.$(O) runtime/libcamlrun_non_shared.$(A)
	$(call MKEXE_VIA_CC,$@,$^ $(BYTECCLIBS))

runtime/libcamlrun.$(A): $(libcamlrun_OBJECTS)
	$(call MKLIB,$@, $^)

runtime/libcamlrun_non_shared.$(A): $(libcamlrun_non_shared_OBJECTS)
	$(call MKLIB,$@, $^)

runtime/ocamlrund$(EXE): runtime/prims.$(O) runtime/libcamlrund.$(A)
	$(MKEXE) $(MKEXEDEBUGFLAG) -o $@ $^ $(BYTECCLIBS)

runtime/libcamlrund.$(A): $(libcamlrund_OBJECTS)
	$(call MKLIB,$@, $^)

runtime/ocamlruni$(EXE): runtime/prims.$(O) runtime/libcamlruni.$(A)
	$(MKEXE) -o $@ $^ $(INSTRUMENTED_RUNTIME_LIBS) $(BYTECCLIBS)

runtime/libcamlruni.$(A): $(libcamlruni_OBJECTS)
	$(call MKLIB,$@, $^)

runtime/libcamlrun_pic.$(A): $(libcamlrunpic_OBJECTS)
	$(call MKLIB,$@, $^)

runtime/libcamlrun_shared.$(SO): $(libcamlrunpic_OBJECTS)
	$(MKDLL) -o $@ $^ $(BYTECCLIBS)

runtime/libasmrun.$(A): $(libasmrun_OBJECTS)
	$(call MKLIB,$@, $^)

runtime/libasmrund.$(A): $(libasmrund_OBJECTS)
	$(call MKLIB,$@, $^)

runtime/libasmruni.$(A): $(libasmruni_OBJECTS)
	$(call MKLIB,$@, $^)

runtime/libasmrun_pic.$(A): $(libasmrunpic_OBJECTS)
	$(call MKLIB,$@, $^)

runtime/libasmrun_shared.$(SO): $(libasmrunpic_OBJECTS)
	$(MKDLL) -o $@ $^ $(NATIVECCLIBS)

## Runtime target-specific preprocessor and compiler flags

runtime/%.$(O): OC_CPPFLAGS += $(runtime_CPPFLAGS)
$(DEPDIR)/runtime/%.$(D): OC_CPPFLAGS += $(runtime_CPPFLAGS)

runtime/%.bd.$(O): OC_CPPFLAGS += $(ocamlrund_CPPFLAGS)
$(DEPDIR)/runtime/%.bd.$(D): OC_CPPFLAGS += $(ocamlrund_CPPFLAGS)

runtime/%.bi.$(O): OC_CPPFLAGS += $(ocamlruni_CPPFLAGS)
$(DEPDIR)/runtime/%.bi.$(D): OC_CPPFLAGS += $(ocamlruni_CPPFLAGS)

runtime/%.bpic.$(O): OC_CFLAGS += $(SHAREDLIB_CFLAGS)
$(DEPDIR)/runtime/%.bpic.$(D): OC_CFLAGS += $(SHAREDLIB_CFLAGS)

runtime/%.n.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS)
$(DEPDIR)/runtime/%.n.$(D): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS)

runtime/%.nd.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(ocamlrund_CPPFLAGS)
$(DEPDIR)/runtime/%.nd.$(D): \
  OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(ocamlrund_CPPFLAGS)

runtime/%.ni.$(O): OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(ocamlruni_CPPFLAGS)
$(DEPDIR)/runtime/%.ni.$(D): \
  OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(ocamlruni_CPPFLAGS)

runtime/%.npic.$(O): OC_CFLAGS += $(OC_NATIVE_CPPFLAGS) $(SHAREDLIB_CFLAGS)
$(DEPDIR)/runtime/%.npic.$(D): \
  OC_CPPFLAGS += $(OC_NATIVE_CPPFLAGS) $(SHAREDLIB_CFLAGS)

## Compilation of runtime C files

# The COMPILE_C_FILE macro below receives as argument the pattern
# that corresponds to the name of the generated object file
# (without the extension, which is added by the macro)
define COMPILE_C_FILE
ifeq "$(COMPUTE_DEPS)" "true"
ifneq "$(1)" "%"
# -MG would ensure that the dependencies are generated even if the files listed
# in $$(runtime_BUILT_HEADERS) haven't been assembled yet. However,
# this goes subtly wrong if the user has the headers installed,
# as gcc will pick up a dependency on those instead and the local
# ones will not be generated. For this reason, we don't use -MG and
# instead include $(runtime_BUILT_HEADERS) in the order only dependencies
# to ensure that they exist before dependencies are computed.
$(DEPDIR)/$(1).$(D): runtime/%.c | $(DEPDIR)/runtime $(runtime_BUILT_HEADERS)
	$$(DEP_CC) $$(OC_CPPFLAGS) $$(CPPFLAGS) $$< -MT \
	  'runtime/$$*$(subst runtime/%,,$(1)).$(O)' -MF $$@
endif # ifneq "$(1)" "%"
$(1).$(O): $(2).c
else
$(1).$(O): $(2).c \
  $(runtime_CONFIGURED_HEADERS) $(runtime_BUILT_HEADERS) \
  $(RUNTIME_HEADERS)
endif # ifeq "$(COMPUTE_DEPS)" "true"
	$$(CC) -c $$(OC_CFLAGS) $$(CFLAGS) $$(OC_CPPFLAGS) $$(CPPFLAGS) \
	  $$(OUTPUTOBJ)$$@ $$<
endef

$(DEPDIR)/runtime:
	$(MKDIR) $@

runtime_OBJECT_TYPES = % %.b %.bd %.bi %.bpic
ifeq "$(NATIVE_COMPILER)" "true"
runtime_OBJECT_TYPES += %.n %.nd %.ni %.np %.npic
endif

$(foreach runtime_OBJECT_TYPE, $(runtime_OBJECT_TYPES), \
  $(eval $(call COMPILE_C_FILE,runtime/$(runtime_OBJECT_TYPE),runtime/%)))

runtime/$(UNIX_OR_WIN32)_non_shared.%.$(O): \
  OC_CPPFLAGS += -DBUILDING_LIBCAMLRUNS

$(eval $(call COMPILE_C_FILE,runtime/$(UNIX_OR_WIN32)_non_shared.%, \
  runtime/$(UNIX_OR_WIN32)))

$(foreach runtime_OBJECT_TYPE,$(subst %,,$(runtime_OBJECT_TYPES)), \
  $(eval \
    runtime/dynlink$(runtime_OBJECT_TYPE).$(O): $(ROOTDIR)/Makefile.config))

## Compilation of runtime assembly files

ASPP_ERROR = \
  { echo "If your assembler produced syntax errors, it is probably";\
          echo "unhappy with the preprocessor. Check your assembler, or";\
          echo "try producing $*.o by hand.";\
          exit 2; }
runtime/%.o: runtime/%.S
	$(ASPP) $(OC_ASPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%.d.o: runtime/%.S
	$(ASPP) $(OC_ASPPFLAGS) $(OC_DEBUG_CPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%.i.o: runtime/%.S
	$(ASPP) $(OC_ASPPFLAGS) $(OC_INSTR_CPPFLAGS) -o $@ $< || $(ASPP_ERROR)

runtime/%_libasmrunpic.o: runtime/%.S
	$(ASPP) $(OC_ASPPFLAGS) $(SHAREDLIB_CFLAGS) -o $@ $<

runtime/domain_state64.inc: \
  runtime/gen_domain_state64_inc.awk runtime/caml/domain_state.tbl
	$(AWK) -f $^ > $@

runtime/domain_state32.inc: \
  runtime/gen_domain_state32_inc.awk runtime/caml/domain_state.tbl
	$(AWK) -f $^ > $@

runtime/amd64nt.obj: runtime/amd64nt.asm runtime/domain_state64.inc
	$(ASM)$@ $<

runtime/i386nt.obj: runtime/i386nt.asm runtime/domain_state32.inc
	$(ASM)$@ $<

runtime/amd64nt.d.obj: runtime/amd64nt.asm runtime/domain_state64.inc
	$(ASM)$@ $(ocamlrund_CPPFLAGS) $<

runtime/i386nt.d.obj: runtime/i386nt.asm runtime/domain_state32.inc
	$(ASM)$@ $(ocmalrund_CPPFLAGS) $<

runtime/amd64nt.i.obj: runtime/amd64nt.asm runtime/domain_state64.inc
	$(ASM)$@ $(ocamlruni_CPPFLAGS) $<

runtime/i386nt.i.obj: runtime/i386nt.asm runtime/domain_state32.inc
	$(ASM)$@ $(ocamlruni_CPPFLAGS) $<

runtime/%_libasmrunpic.obj: runtime/%.asm
	$(ASM)$@ $<

## Runtime dependencies

runtime_DEP_FILES := $(addsuffix .b, \
  $(basename $(runtime_BYTECODE_C_SOURCES) runtime/instrtrace))
ifeq "$(NATIVE_COMPILER)" "true"
runtime_DEP_FILES += $(addsuffix .n, $(basename $(runtime_NATIVE_C_SOURCES)))
endif
runtime_DEP_FILES += $(addsuffix d, $(runtime_DEP_FILES)) \
             $(addsuffix i, $(runtime_DEP_FILES)) \
             $(addsuffix pic, $(runtime_DEP_FILES))
runtime_DEP_FILES := $(addsuffix .$(D), $(runtime_DEP_FILES))

ifeq "$(COMPUTE_DEPS)" "true"
include $(addprefix $(DEPDIR)/, $(runtime_DEP_FILES))
endif

.PHONY: runtime
runtime: stdlib/libcamlrun.$(A)

ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
runtime: $(addprefix stdlib/flexdll/, $(FLEXDLL_OBJECTS))
stdlib/flexdll/flexdll%.$(O): $(FLEXDLL_SOURCES)/flexdll%.$(O) | stdlib/flexdll
	cp $< $@
stdlib/flexdll:
	$(MKDIR) $@
endif

.PHONY: makeruntime
makeruntime: runtime-all
stdlib/libcamlrun.$(A): runtime-all
	cd stdlib; $(LN) ../runtime/libcamlrun.$(A) .
clean::
	rm -f $(addprefix runtime/, *.o *.obj *.a *.lib *.so *.dll ld.conf)
	rm -f $(addprefix runtime/, ocamlrun ocamlrund ocamlruni ocamlruns sak)
	rm -f $(addprefix runtime/, \
	  ocamlrun.exe ocamlrund.exe ocamlruni.exe ocamlruns.exe sak.exe)
	rm -f runtime/primitives runtime/primitives.new runtime/prims.c \
	  $(runtime_BUILT_HEADERS)
	rm -f runtime/domain_state*.inc
	rm -rf $(DEPDIR)
	rm -f stdlib/libcamlrun.a stdlib/libcamlrun.lib

.PHONY: runtimeopt
runtimeopt: stdlib/libasmrun.$(A)

.PHONY: makeruntimeopt
makeruntimeopt: runtime-allopt
stdlib/libasmrun.$(A): runtime-allopt
	cd stdlib; $(LN) ../runtime/libasmrun.$(A) .

clean::
	rm -f stdlib/libasmrun.a stdlib/libasmrun.lib

# Dependencies

subdirs = stdlib $(addprefix otherlibs/, $(ALL_OTHERLIBS)) \
  debugger ocamldoc ocamltest tools

.PHONY: alldepend
alldepend: depend
	for dir in $(subdirs); do \
	  $(MAKE) -C $$dir depend || exit; \
	done

# The standard library

.PHONY: library
library: ocamlc
	$(MAKE) -C stdlib all

.PHONY: library-cross
library-cross:
	$(MAKE) -C stdlib OCAMLRUN=../runtime/ocamlrun$(EXE) all

.PHONY: libraryopt
libraryopt:
	$(MAKE) -C stdlib allopt

partialclean::
	$(MAKE) -C stdlib clean

# The lexer generator

ocamllex_MODULES = $(addprefix lex/,\
  cset syntax parser lexer table lexgen compact  common output outputbis main)

.PHONY: lex-all
lex-all: lex/ocamllex

.PHONY: lex-allopt
lex-allopt: lex/ocamllex.opt

.PHONY: ocamllex
ocamllex: ocamlyacc
	$(MAKE) lex-all

.PHONY: ocamllex.opt
ocamllex.opt: ocamlopt
	$(MAKE) lex-allopt

lex/ocamllex$(EXE): $(ocamllex_MODULES:=.cmo)
	$(CAMLC) $(OC_COMMON_LDFLAGS) -compat-32 -o $@ $^

lex/ocamllex.opt$(EXE): $(ocamllex_MODULES:=.cmx)
	$(CAMLOPT_CMD) $(OC_COMMON_LDFLAGS) -o $@ $^

partialclean::
	rm -f lex/*.cm* lex/*.o lex/*.obj

beforedepend:: lex/parser.ml lex/parser.mli lex/lexer.ml

clean::
	rm -f lex/parser.ml lex/parser.mli lex/parser.output
	rm -f lex/lexer.ml

# The ocamlyacc parser generator

ocamlyacc_OTHER_MODULES = $(addprefix yacc/,\
  closure error lalr lr0 main mkpar output reader skeleton symtab \
  verbose warshall)

ocamlyacc_MODULES = $(ocamlyacc_WSTR_MODULE) $(ocamlyacc_OTHER_MODULES)

ocamlyacc_OBJECTS = $(ocamlyacc_MODULES:=.$(O))

# Do not compile assertions in ocamlyacc
ocamlyacc_CPPFLAGS = -DNDEBUG

.PHONY: ocamlyacc
ocamlyacc: $(ocamlyacc_PROGRAM)$(EXE)

$(ocamlyacc_PROGRAM)$(EXE): $(ocamlyacc_OBJECTS)
	$(MKEXE) -o $@ $^

clean::
	rm -f $(ocamlyacc_MODULES:=.o) $(ocamlyacc_MODULES:=.obj)

$(ocamlyacc_OTHER_MODULES:=.$(O)): yacc/defs.h

$(ocamlyacc_OTHER_MODULES:=.$(O)): OC_CPPFLAGS += $(ocamlyacc_CPPFLAGS)

# The Menhir-generated parser

# In order to avoid a build-time dependency on Menhir,
# we store the result of the parser generator (which
# are OCaml source files) and Menhir's runtime libraries
# (that the parser files rely on) in boot/.

# The rules below do not depend on Menhir being available,
# they just build the parser from boot/.

# See Makefile.menhir for the rules to rebuild the parser and update
# boot/, which require Menhir. The targets in Makefile.menhir
# (also included here for convenience) must be used after any
# modification of parser.mly.
include Makefile.menhir

# To avoid module-name conflicts with compiler-lib users that link
# with their code with their own MenhirLib module (possibly with
# a different Menhir version), we rename MenhirLib into
# CamlinternalMenhirlib -- and replace the module occurrences in the
# generated parser.ml.

parsing/camlinternalMenhirLib.ml: boot/menhir/menhirLib.ml
	cp $< $@
parsing/camlinternalMenhirLib.mli: boot/menhir/menhirLib.mli
	echo '[@@@ocaml.warning "-67"]' > $@
	cat $< >> $@

# Copy parsing/parser.ml from boot/

PARSER_DEPS = boot/menhir/parser.ml parsing/parser.mly

ifeq "$(OCAML_DEVELOPMENT_VERSION)" "true"
PARSER_DEPS += tools/check-parser-uptodate-or-warn.sh
endif

parsing/parser.ml: $(PARSER_DEPS)
ifeq "$(OCAML_DEVELOPMENT_VERSION)" "true"
	@-tools/check-parser-uptodate-or-warn.sh
endif
	sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@
parsing/parser.mli: boot/menhir/parser.mli
	sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@

beforedepend:: parsing/camlinternalMenhirLib.ml \
  parsing/camlinternalMenhirLib.mli \
  parsing/parser.ml parsing/parser.mli

partialclean:: partialclean-menhir


# OCamldoc

.PHONY: ocamldoc
ocamldoc: ocamlc ocamlyacc ocamllex otherlibraries
	$(MAKE) -C ocamldoc all

.PHONY: ocamldoc.opt
ocamldoc.opt: ocamlc.opt ocamlyacc ocamllex
	$(MAKE) -C ocamldoc opt.opt

# OCamltest
ocamltest: ocamlc ocamlyacc ocamllex otherlibraries
	$(MAKE) -C ocamltest all

ocamltest.opt: ocamlc.opt ocamlyacc ocamllex
	$(MAKE) -C ocamltest allopt

partialclean::
	$(MAKE) -C ocamltest clean

# Documentation

.PHONY: html_doc
html_doc: ocamldoc
	$(MAKE) -C api_docgen html

.PHONY: manpages
manpages:
	$(MAKE) -C api_docgen man

partialclean::
	$(MAKE) -C ocamldoc clean

partialclean::
	$(MAKE) -C api_docgen clean

# The extra libraries

.PHONY: otherlibraries
otherlibraries: ocamltools
	$(MAKE) -C otherlibs all

.PHONY: otherlibrariesopt
otherlibrariesopt:
	$(MAKE) -C otherlibs allopt

partialclean::
	$(MAKE) -C otherlibs partialclean

clean::
	$(MAKE) -C otherlibs clean

# The replay debugger

.PHONY: ocamldebugger
ocamldebugger: ocamlc ocamlyacc ocamllex otherlibraries
	$(MAKE) -C debugger all

partialclean::
	$(MAKE) -C debugger clean

# Check that the native-code compiler is supported
.PHONY: checknative
checknative:
ifeq "$(NATIVE_COMPILER)" "false"
	$(error The source tree was configured with --disable-native-compiler!)
else
ifeq "$(ARCH)" "none"
	$(error The native-code compiler is not supported on this platform)
else
	@
endif
endif

# Check that the stack limit is reasonable (Unix-only)
.PHONY: checkstack
ifeq "$(UNIX_OR_WIN32)" "unix"
checkstack: tools/checkstack$(EXE)
	$<

.INTERMEDIATE: tools/checkstack$(EXE) tools/checkstack.$(O)
tools/checkstack$(EXE): tools/checkstack.$(O)
	$(MAKE) -C tools checkstack$(EXE)
else
checkstack:
	@
endif

# Lint @since and @deprecated annotations

VERSIONS=$(shell git tag|grep '^[0-9]*.[0-9]*.[0-9]*$$'|grep -v '^[12].')
.PHONY: lintapidiff
lintapidiff:
	$(MAKE) -C tools lintapidiff.opt
	git ls-files -- 'otherlibs/*/*.mli' 'stdlib/*.mli' |\
	    grep -Ev internal\|obj\|stdLabels\|moreLabels |\
	    tools/lintapidiff.opt $(VERSIONS)

# Tools

.PHONY: ocamltools
ocamltools: ocamlc ocamllex compilerlibs/ocamlmiddleend.cma
	$(MAKE) -C tools all

.PHONY: ocamltoolsopt
ocamltoolsopt: ocamlopt
	$(MAKE) -C tools opt

.PHONY: ocamltoolsopt.opt
ocamltoolsopt.opt: ocamlc.opt ocamllex.opt compilerlibs/ocamlmiddleend.cmxa
	$(MAKE) -C tools opt.opt

# tools that require a full ocaml distribution: otherlibs and toplevel
.PHONY:othertools
othertools:
	$(MAKE) -C tools othertools

partialclean::
	$(MAKE) -C tools clean

## Test compilation of backend-specific parts

ARCH_SPECIFIC =\
  asmcomp/arch.mli asmcomp/arch.ml asmcomp/proc.ml asmcomp/CSE.ml \
  asmcomp/selection.ml asmcomp/scheduling.ml asmcomp/reload.ml

partialclean::
	rm -f $(ARCH_SPECIFIC)

beforedepend:: $(ARCH_SPECIFIC)

# This rule provides a quick way to check that machine-dependent
# files compiles fine for a foreign architecture (passed as ARCH=xxx).

.PHONY: check_arch
check_arch:
	@echo "========= CHECKING asmcomp/$(ARCH) =============="
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*
	@$(MAKE) compilerlibs/ocamloptcomp.cma \
	            >/dev/null
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*

.PHONY: check_all_arches
check_all_arches:
ifeq ($(ARCH64),true)
	@STATUS=0; \
	 for i in $(ARCHES); do \
	   $(MAKE) --no-print-directory check_arch ARCH=$$i || STATUS=1; \
	 done; \
	 exit $$STATUS
else
	 @echo "Architecture tests are disabled on 32-bit platforms."
endif

# The native toplevel

ocamlnat_dependencies := \
  compilerlibs/ocamlcommon.cmxa \
  compilerlibs/ocamloptcomp.cmxa \
  compilerlibs/ocamlbytecomp.cmxa \
  otherlibs/dynlink/dynlink.cmxa \
  compilerlibs/ocamltoplevel.cmxa \
  $(TOPLEVELSTART:.cmo=.cmx)

ocamlnat$(EXE): $(ocamlnat_dependencies)
	$(CAMLOPT_CMD) $(OC_COMMON_LDFLAGS) -linkall -I toplevel/native -o $@ $^

COMPILE_NATIVE_MODULE = \
  $(CAMLOPT_CMD) $(OC_COMMON_CFLAGS) -I $(@D) $(INCLUDES) $(OC_NATIVE_CFLAGS)

toplevel/topdirs.cmx toplevel/toploop.cmx $(TOPLEVELSTART:.cmo=.cmx): \
  OC_NATIVE_CFLAGS += -I toplevel/native

$(TOPLEVELINIT:.cmo=.cmx): toplevel/native/topeval.cmx

$(TOPLEVELSTART:.cmo=.cmx): toplevel/native/topmain.cmx

partialclean::
	rm -f ocamlnat ocamlnat.exe

toplevel/native/topeval.cmx: otherlibs/dynlink/dynlink.cmxa

# The numeric opcodes

make_opcodes := tools/make_opcodes$(EXE)

bytecomp/opcodes.ml: runtime/caml/instruct.h $(make_opcodes)
	$(NEW_OCAMLRUN) $(make_opcodes) -opcodes < $< > $@

bytecomp/opcodes.mli: bytecomp/opcodes.ml
	$(CAMLC) -i $< > $@

$(make_opcodes): tools/make_opcodes.mll
	$(MAKE) -C tools make_opcodes

partialclean::
	rm -f bytecomp/opcodes.ml
	rm -f bytecomp/opcodes.mli

beforedepend:: bytecomp/opcodes.ml bytecomp/opcodes.mli

ifneq "$(wildcard .git)" ""
include Makefile.dev
endif

# Default rules

%.cmo: %.ml
	$(CAMLC) $(OC_COMMON_CFLAGS) -I $(@D) $(INCLUDES) -c $<

%.cmi: %.mli
	$(CAMLC) $(OC_COMMON_CFLAGS) -I $(@D) $(INCLUDES) -c $<

%.cmx: %.ml
	$(COMPILE_NATIVE_MODULE) -c $<

partialclean::
	for d in utils parsing typing bytecomp asmcomp middle_end file_formats \
           lambda middle_end/closure middle_end/flambda \
           middle_end/flambda/base_types \
           driver toplevel toplevel/byte toplevel/native tools; do \
	  rm -f $$d/*.cm[ioxt] $$d/*.cmti $$d/*.annot $$d/*.s $$d/*.asm \
	    $$d/*.o $$d/*.obj $$d/*.so $$d/*.dll; \
	done

.PHONY: depend
depend: beforedepend
	(for d in utils parsing typing bytecomp asmcomp middle_end \
         lambda file_formats middle_end/closure middle_end/flambda \
         middle_end/flambda/base_types \
         driver toplevel toplevel/byte toplevel/native lex; \
	 do \
	   $(OCAMLDEP) $(OC_OCAMLDEPFLAGS) -I $$d $(INCLUDES) \
	   $(OCAMLDEPFLAGS) $$d/*.mli $$d/*.ml \
	   || exit; \
         done) > .depend

.PHONY: distclean
distclean: clean
	$(MAKE) -C debugger distclean
	$(MAKE) -C manual distclean
	$(MAKE) -C ocamldoc distclean
	$(MAKE) -C ocamltest distclean
	$(MAKE) -C otherlibs distclean
	rm -f $(runtime_CONFIGURED_HEADERS)
	$(MAKE) -C stdlib distclean
	$(MAKE) -C testsuite distclean
	$(MAKE) -C tools distclean
	rm -f utils/config.generated.ml
	rm -f compilerlibs/META
	rm -f boot/ocamlrun boot/ocamlrun.exe boot/camlheader \
	      boot/ocamlruns boot/ocamlruns.exe \
	      boot/flexlink.byte boot/flexlink.byte.exe \
	      boot/flexdll_*.o boot/flexdll_*.obj \
	      boot/*.cm* boot/libcamlrun.a boot/libcamlrun.lib boot/ocamlc.opt
	rm -f Makefile.config Makefile.build_config
	rm -rf autom4te.cache flexdll-sources
	rm -f config.log config.status libtool

# Installation
.PHONY: install
install:
	$(MKDIR) "$(INSTALL_BINDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR)"
	$(MKDIR) "$(INSTALL_STUBLIBDIR)"
	$(MKDIR) "$(INSTALL_COMPLIBDIR)"
	$(MKDIR) "$(INSTALL_DOCDIR)"
	$(MKDIR) "$(INSTALL_INCDIR)"
	$(INSTALL_PROG) $(runtime_PROGRAMS) "$(INSTALL_BINDIR)"
	$(INSTALL_DATA) $(runtime_BYTECODE_STATIC_LIBRARIES) \
	  "$(INSTALL_LIBDIR)"
ifneq "$(runtime_BYTECODE_SHARED_LIBRARIES)" ""
	$(INSTALL_PROG) $(runtime_BYTECODE_SHARED_LIBRARIES) \
	  "$(INSTALL_LIBDIR)"
endif
	$(INSTALL_DATA) runtime/caml/domain_state.tbl runtime/caml/*.h \
	  "$(INSTALL_INCDIR)"
	$(INSTALL_PROG) ocaml$(EXE) "$(INSTALL_BINDIR)"
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) ocamlc$(EXE) "$(INSTALL_BINDIR)/ocamlc.byte$(EXE)"
endif
	$(MAKE) -C stdlib install
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) lex/ocamllex$(EXE) \
	  "$(INSTALL_BINDIR)/ocamllex.byte$(EXE)"
endif
	$(INSTALL_PROG) $(ocamlyacc_PROGRAM)$(EXE) "$(INSTALL_BINDIR)"
	$(INSTALL_DATA) \
	   utils/*.cmi \
	   parsing/*.cmi \
	   typing/*.cmi \
	   bytecomp/*.cmi \
	   file_formats/*.cmi \
	   lambda/*.cmi \
	   driver/*.cmi \
	   toplevel/*.cmi \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   toplevel/byte/*.cmi \
	   "$(INSTALL_COMPLIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   utils/*.cmt utils/*.cmti utils/*.mli \
	   parsing/*.cmt parsing/*.cmti parsing/*.mli \
	   typing/*.cmt typing/*.cmti typing/*.mli \
	   file_formats/*.cmt file_formats/*.cmti file_formats/*.mli \
	   lambda/*.cmt lambda/*.cmti lambda/*.mli \
	   bytecomp/*.cmt bytecomp/*.cmti bytecomp/*.mli \
	   driver/*.cmt driver/*.cmti driver/*.mli \
	   toplevel/*.cmt toplevel/*.cmti toplevel/*.mli \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   toplevel/byte/*.cmt \
	   "$(INSTALL_COMPLIBDIR)"
endif
	$(INSTALL_DATA) \
	  compilerlibs/*.cma compilerlibs/META \
	  "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   $(BYTESTART) $(TOPLEVELSTART) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_PROG) $(expunge) "$(INSTALL_LIBDIR)"
# If installing over a previous OCaml version, ensure the module is removed
# from the previous installation.
	rm -f "$(INSTALL_LIBDIR)"/topdirs.cm* "$(INSTALL_LIBDIR)/topdirs.mli"
	$(MAKE) -C tools install
ifeq "$(UNIX_OR_WIN32)" "unix" # Install manual pages only on Unix
	$(MAKE) -C man install
endif
	for i in $(OTHERLIBRARIES); do \
	  $(MAKE) -C otherlibs/$$i install || exit $$?; \
	done
ifeq "$(build_ocamldoc)" "true"
	$(MAKE) -C ocamldoc install
endif
ifeq "$(WITH_OCAMLDOC)-$(STDLIB_MANPAGES)" "ocamldoc-true"
	$(MAKE) -C api_docgen install
endif
	if test -n "$(WITH_DEBUGGER)"; then \
	  $(MAKE) -C debugger install; \
	fi
ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
ifeq "$(TOOLCHAIN)" "msvc"
	$(INSTALL_DATA) $(FLEXDLL_SOURCES)/$(FLEXDLL_MANIFEST) \
    "$(INSTALL_BINDIR)/"
endif
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) \
	  boot/flexlink.byte$(EXE) "$(INSTALL_BINDIR)/flexlink.byte$(EXE)"
endif # ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(MKDIR) "$(INSTALL_FLEXDLLDIR)"
	$(INSTALL_DATA) $(addprefix stdlib/flexdll/, $(FLEXDLL_OBJECTS)) \
    "$(INSTALL_FLEXDLLDIR)"
endif # ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
	$(INSTALL_DATA) Makefile.config "$(INSTALL_LIBDIR)"
	$(INSTALL_DATA) $(DOC_FILES) "$(INSTALL_DOCDIR)"
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	if test -f ocamlopt$(EXE); then $(MAKE) installopt; else \
	   cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.byte$(EXE) ocamlc$(EXE); \
	   $(LN) ocamllex.byte$(EXE) ocamllex$(EXE); \
	   (test -f flexlink.byte$(EXE) && \
	      $(LN) flexlink.byte$(EXE) flexlink$(EXE)) || true; \
	fi
else
	if test -f ocamlopt$(EXE); then $(MAKE) installopt; fi
endif

# Installation of the native-code compiler
.PHONY: installopt
installopt:
	$(INSTALL_DATA) $(runtime_NATIVE_STATIC_LIBRARIES) "$(INSTALL_LIBDIR)"
ifneq "$(runtime_NATIVE_SHARED_LIBRARIES)" ""
	$(INSTALL_PROG) $(runtime_NATIVE_SHARED_LIBRARIES) "$(INSTALL_LIBDIR)"
endif
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) ocamlopt$(EXE) "$(INSTALL_BINDIR)/ocamlopt.byte$(EXE)"
endif
	$(MAKE) -C stdlib installopt
	$(INSTALL_DATA) \
	    middle_end/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/closure/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/base_types/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    asmcomp/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	    middle_end/*.cmt middle_end/*.cmti \
	    middle_end/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/closure/*.cmt middle_end/closure/*.cmti \
	    middle_end/closure/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/*.cmt middle_end/flambda/*.cmti \
	    middle_end/flambda/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/base_types/*.cmt \
            middle_end/flambda/base_types/*.cmti \
	    middle_end/flambda/base_types/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    asmcomp/*.cmt asmcomp/*.cmti \
	    asmcomp/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
endif
	$(INSTALL_DATA) \
	    $(OPTSTART) \
	    "$(INSTALL_COMPLIBDIR)"
ifeq "$(build_ocamldoc)" "true"
	$(MAKE) -C ocamldoc installopt
endif
	for i in $(OTHERLIBRARIES); do \
	  $(MAKE) -C otherlibs/$$i installopt || exit $$?; \
	done
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	if test -f ocamlopt.opt$(EXE); then $(MAKE) installoptopt; else \
	   cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.byte$(EXE) ocamlc$(EXE); \
	   $(LN) ocamlopt.byte$(EXE) ocamlopt$(EXE); \
	   $(LN) ocamllex.byte$(EXE) ocamllex$(EXE); \
	   (test -f flexlink.byte$(EXE) && \
	     $(LN) flexlink.byte$(EXE) flexlink$(EXE)) || true; \
	fi
else
	if test -f ocamlopt.opt$(EXE); then $(MAKE) installoptopt; fi
endif
	$(MAKE) -C tools installopt

.PHONY: installoptopt
installoptopt:
	$(INSTALL_PROG) ocamlc.opt$(EXE) "$(INSTALL_BINDIR)"
	$(INSTALL_PROG) ocamlopt.opt$(EXE) "$(INSTALL_BINDIR)"
	$(INSTALL_PROG) lex/ocamllex.opt$(EXE) "$(INSTALL_BINDIR)"
	cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.opt$(EXE) ocamlc$(EXE); \
	   $(LN) ocamlopt.opt$(EXE) ocamlopt$(EXE); \
	   $(LN) ocamllex.opt$(EXE) ocamllex$(EXE)
ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
	$(INSTALL_PROG) flexlink.opt$(EXE) "$(INSTALL_BINDIR)"
	cd "$(INSTALL_BINDIR)"; \
	  $(LN) flexlink.opt$(EXE) flexlink$(EXE)
endif
	$(INSTALL_DATA) \
	   utils/*.cmx parsing/*.cmx typing/*.cmx bytecomp/*.cmx \
	   toplevel/*.cmx toplevel/native/*.cmx \
	   toplevel/native/tophooks.cmi \
	   file_formats/*.cmx \
	   lambda/*.cmx \
	   driver/*.cmx asmcomp/*.cmx middle_end/*.cmx \
           middle_end/closure/*.cmx \
           middle_end/flambda/*.cmx \
           middle_end/flambda/base_types/*.cmx \
          "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   compilerlibs/*.cmxa compilerlibs/*.$(A) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   $(BYTESTART:.cmo=.cmx) $(BYTESTART:.cmo=.$(O)) \
	   $(OPTSTART:.cmo=.cmx) $(OPTSTART:.cmo=.$(O)) \
	   $(TOPLEVELSTART:.cmo=.$(O)) \
	   "$(INSTALL_COMPLIBDIR)"
ifeq "$(INSTALL_OCAMLNAT)" "true"
	  $(INSTALL_PROG) ocamlnat$(EXE) "$(INSTALL_BINDIR)"
endif

# Installation of the *.ml sources of compiler-libs
.PHONY: install-compiler-sources
install-compiler-sources:
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   utils/*.ml parsing/*.ml typing/*.ml bytecomp/*.ml driver/*.ml \
           file_formats/*.ml \
           lambda/*.ml \
	   toplevel/*.ml toplevel/byte/*.ml \
	   middle_end/*.ml middle_end/closure/*.ml \
     middle_end/flambda/*.ml middle_end/flambda/base_types/*.ml \
	   asmcomp/*.ml \
	   asmcmp/debug/*.ml \
	   "$(INSTALL_COMPLIBDIR)"
endif

include .depend

Makefile.config Makefile.build_config: config.status
config.status:
	@echo "Please refer to the installation instructions:"
	@echo "- In file INSTALL for Unix systems."
	@echo "- In file README.win32.adoc for Windows systems."
	@echo "On Unix systems, if you've just unpacked the distribution,"
	@echo "something like"
	@echo "  ./configure"
	@echo "  make"
	@echo "  make install"
	@echo "should work."
	@false
