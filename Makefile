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
# NOTE: it is important that OCAMLLEX is defined *before* Makefile.common
# gets included, so that its definition here takes precedence
# over the one there.
OCAMLLEX ?= $(BOOT_OCAMLLEX)
include Makefile.common

.PHONY: defaultentry
ifeq "$(NATIVE_COMPILER)" "true"
defaultentry: world.opt
else
defaultentry: world
endif

ifeq "$(UNIX_OR_WIN32)" "win32"
LN = cp
else
LN = ln -sf
endif

include stdlib/StdlibModules

CAMLC=$(BOOT_OCAMLC) -g -nostdlib -I boot -use-prims runtime/primitives
CAMLOPT=$(OCAMLRUN) ./ocamlopt$(EXE) -g -nostdlib -I stdlib -I otherlibs/dynlink
ARCHES=amd64 i386 arm arm64 power s390x riscv
INCLUDES=-I utils -I parsing -I typing -I bytecomp -I file_formats \
        -I lambda -I middle_end -I middle_end/closure \
        -I middle_end/flambda -I middle_end/flambda/base_types \
        -I asmcomp \
        -I driver -I toplevel

COMPFLAGS=-strict-sequence -principal -absname \
          -w +a-4-9-40-41-42-44-45-48-66-70 \
          -warn-error +a \
          -bin-annot -safe-string -strict-formats $(INCLUDES)
LINKFLAGS=

ifeq "$(strip $(NATDYNLINKOPTS))" ""
OCAML_NATDYNLINKOPTS=
else
OCAML_NATDYNLINKOPTS = -ccopt "$(NATDYNLINKOPTS)"
endif

CAMLDEP=$(OCAMLRUN) boot/ocamlc -depend
DEPFLAGS=-slash
DEPINCLUDES=$(INCLUDES)

OCAMLDOC_OPT=$(WITH_OCAMLDOC:=.opt)
OCAMLTEST_OPT=$(WITH_OCAMLTEST:=.opt)

BYTESTART=driver/main.cmo

OPTSTART=driver/optmain.cmo

TOPLEVELSTART=toplevel/topstart.cmo

TOPLEVELINIT=toplevel/toploop.cmo

# This list is passed to expunge, which accepts both uncapitalized and
# capitalized module names.
PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

COMPLIBDIR=$(LIBDIR)/compiler-libs

TOPINCLUDES=$(addprefix -I otherlibs/,$(filter-out %threads,$(OTHERLIBRARIES)))
ifeq "$(UNIX_OR_WIN32)" "unix"
EXTRAPATH=
else
EXTRAPATH = PATH="otherlibs/win32unix:$(PATH)"
endif


ifeq "$(BOOTSTRAPPING_FLEXDLL)" "false"
  COLDSTART_DEPS =
  BOOT_FLEXLINK_CMD =
else
  COLDSTART_DEPS = boot/ocamlruns$(EXE)
  BOOT_FLEXLINK_CMD = \
    FLEXLINK_CMD="../boot/ocamlruns$(EXE) ../boot/flexlink.byte$(EXE)"
endif

expunge := expunge$(EXE)

# targets for the compilerlibs/*.{cma,cmxa} archives
include compilerlibs/Makefile.compilerlibs

# The configuration file

utils/config.ml: utils/config.mlp Makefile.config utils/Makefile
	$(MAKE) -C utils config.ml

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
	rm -f utils/config.ml utils/domainstate.ml utils/domainstate.mli

.PHONY: beforedepend
beforedepend:: utils/config.ml utils/domainstate.ml utils/domainstate.mli

programs := expunge ocaml ocamlc ocamlc.opt ocamlnat ocamlopt ocamlopt.opt

$(foreach program, $(programs), $(eval $(call PROGRAM_SYNONYM,$(program))))

USE_RUNTIME_PRIMS = -use-prims ../runtime/primitives
USE_STDLIB = -nostdlib -I ../stdlib

FLEXDLL_OBJECTS = \
  flexdll_$(FLEXDLL_CHAIN).$(O) flexdll_initer_$(FLEXDLL_CHAIN).$(O)
FLEXLINK_BUILD_ENV = \
  MSVC_DETECT=0 OCAML_CONFIG_FILE=../Makefile.config \
  CHAINS=$(FLEXDLL_CHAIN) ROOTDIR=..

boot/ocamlruns$(EXE):
	$(MAKE) -C runtime ocamlruns$(EXE)
	cp runtime/ocamlruns$(EXE) boot/ocamlruns$(EXE)

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
	$(MAKE) -C runtime all
	$(MAKE) -C stdlib \
	  OCAMLRUN='$$(ROOTDIR)/runtime/ocamlrun$(EXE)' \
	  CAMLC='$$(BOOT_OCAMLC) $(USE_RUNTIME_PRIMS)' all
else
	$(MAKE) -C stdlib OCAMLRUN='$$(ROOTDIR)/boot/ocamlruns$(EXE)' \
    CAMLC='$$(BOOT_OCAMLC)' all
	$(MAKE) -C $(FLEXDLL_SOURCES) $(FLEXLINK_BUILD_ENV) \
	  OCAMLRUN='$$(ROOTDIR)/boot/ocamlruns$(EXE)' NATDYNLINK=false \
	  OCAMLOPT='$(value BOOT_OCAMLC) $(USE_RUNTIME_PRIMS) $(USE_STDLIB)' \
	  flexlink.exe support
	mv $(FLEXDLL_SOURCES)/flexlink.exe boot/flexlink.byte$(EXE)
	cp $(addprefix $(FLEXDLL_SOURCES)/, $(FLEXDLL_OBJECTS)) boot/
	$(MAKE) -C runtime $(BOOT_FLEXLINK_CMD) all
endif # ifeq "$(BOOTSTRAPPING_FLEXDLL)" "false"
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

CMPBYT := $(OCAMLRUN) tools/cmpbyt$(EXE)

.PHONY: compare
compare:
	@if $(CMPBYT) boot/ocamlc ocamlc$(EXE) \
         && $(CMPBYT) boot/ocamllex lex/ocamllex$(EXE); \
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
	  $(OCAMLTEST_OPT) ocamlnat
ifeq "$(WITH_OCAMLDOC)-$(STDLIB_MANPAGES)" "ocamldoc-true"
	$(MAKE) manpages
endif

# Core bootstrapping cycle
.PHONY: coreboot
coreboot:
# Promote the new compiler but keep the old runtime
# This compiler runs on boot/ocamlrun and produces bytecode for
# runtime/ocamlrun
	$(MAKE) promote-cross
# Rebuild ocamlc and ocamllex (run on runtime/ocamlrun)
	$(MAKE) partialclean
	$(MAKE) ocamlc ocamllex ocamltools
# Rebuild the library (using runtime/ocamlrun ./ocamlc)
	$(MAKE) library-cross
# Promote the new compiler and the new runtime
	$(MAKE) OCAMLRUN=runtime/ocamlrun$(EXE) promote
# Rebuild the core system
	$(MAKE) partialclean
	$(MAKE) core
# Check if fixpoint reached
	$(MAKE) compare

# Recompile the system using the bootstrap compiler

.PHONY: all
all: coreall
	$(MAKE) ocaml
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) \
         $(WITH_OCAMLTEST)
ifeq "$(WITH_OCAMLDOC)-$(STDLIB_MANPAGES)" "ocamldoc-true"
	$(MAKE) manpages
endif

# Bootstrap and rebuild the whole system.
# The compilation of ocaml will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.
.PHONY: bootstrap
bootstrap: coreboot
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

flexlink.opt$(EXE):
	$(MAKE) -C $(FLEXDLL_SOURCES) $(FLEXLINK_BUILD_ENV) \
    OCAML_FLEXLINK='$(value OCAMLRUN) $$(ROOTDIR)/boot/flexlink.byte$(EXE)' \
	  OCAMLOPT="$(FLEXLINK_OCAMLOPT) -nostdlib -I ../stdlib" flexlink.exe
	mv $(FLEXDLL_SOURCES)/flexlink.exe $@

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

# Installation
.PHONY: install
install:
	$(MKDIR) "$(INSTALL_BINDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR)"
	$(MKDIR) "$(INSTALL_STUBLIBDIR)"
	$(MKDIR) "$(INSTALL_COMPLIBDIR)"
	$(MKDIR) "$(INSTALL_DOCDIR)"
	$(MAKE) -C runtime install
	$(INSTALL_PROG) ocaml$(EXE) "$(INSTALL_BINDIR)"
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) ocamlc$(EXE) "$(INSTALL_BINDIR)/ocamlc.byte$(EXE)"
endif
	$(MAKE) -C stdlib install
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) lex/ocamllex$(EXE) \
	  "$(INSTALL_BINDIR)/ocamllex.byte$(EXE)"
endif
	$(INSTALL_PROG) yacc/ocamlyacc$(EXE) "$(INSTALL_BINDIR)"
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
	  compilerlibs/*.cma \
	  "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   $(BYTESTART) $(TOPLEVELSTART) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_PROG) $(expunge) "$(INSTALL_LIBDIR)"
	$(INSTALL_DATA) \
	   toplevel/topdirs.cmi \
	   "$(INSTALL_LIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   toplevel/topdirs.cmt \
	   toplevel/topdirs.cmti toplevel/topdirs.mli \
	   "$(INSTALL_LIBDIR)"
endif
	$(MAKE) -C tools install
ifeq "$(UNIX_OR_WIN32)" "unix" # Install manual pages only on Unix
	$(MAKE) -C man install
endif
	for i in $(OTHERLIBRARIES); do \
	  $(MAKE) -C otherlibs/$$i install || exit $$?; \
	done
ifneq "$(WITH_OCAMLDOC)" ""
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
	$(MAKE) -C runtime installopt
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
ifneq "$(WITH_OCAMLDOC)" ""
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
	rm -f $(programs) $(programs:=.exe)

# The bytecode compiler

ocamlc$(EXE): compilerlibs/ocamlcommon.cma \
              compilerlibs/ocamlbytecomp.cma $(BYTESTART)
	$(CAMLC) $(LINKFLAGS) -compat-32 -o $@ $^

partialclean::
	rm -rf ocamlc$(EXE)

# The native-code compiler

ocamlopt$(EXE): compilerlibs/ocamlcommon.cma compilerlibs/ocamloptcomp.cma \
          $(OPTSTART)
	$(CAMLC) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt$(EXE)

# The toplevel

ocaml_dependencies := \
  compilerlibs/ocamlcommon.cma \
  compilerlibs/ocamlbytecomp.cma \
  compilerlibs/ocamltoplevel.cma $(TOPLEVELSTART)

.INTERMEDIATE: ocaml.tmp
ocaml.tmp: $(ocaml_dependencies)
	$(CAMLC) $(LINKFLAGS) -I toplevel/byte -linkall -o $@ $^

ocaml$(EXE): $(expunge) ocaml.tmp
	- $(OCAMLRUN) $^ $@ $(PERVASIVES)

partialclean::
	rm -f ocaml$(EXE)

# Use TOPFLAGS to pass additional flags to the bytecode or native toplevel
# when running make runtop or make natruntop
TOPFLAGS ?=
OC_TOPFLAGS = -nostdlib -I stdlib -I toplevel -noinit $(TOPINCLUDES) $(TOPFLAGS)

# Note: Beware that, since this rule begins with a coldstart, both
# boot/ocamlrun and runtime/ocamlrun will be the same when the toplevel
# is run.
.PHONY: runtop
runtop:
	$(MAKE) coldstart
	$(MAKE) ocamlc
	$(MAKE) otherlibraries
	$(MAKE) ocaml
	@$(EXTRAPATH) $(RLWRAP) $(OCAMLRUN) ./ocaml$(EXE) $(OC_TOPFLAGS)

.PHONY: natruntop
natruntop:
	$(MAKE) core
	$(MAKE) opt
	$(MAKE) ocamlnat
	@$(FLEXLINK_ENV) $(EXTRAPATH) $(RLWRAP) ./ocamlnat$(EXE) $(OC_TOPFLAGS)

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
	$(CAMLOPT_CMD) $(LINKFLAGS) -o $@ $^ -cclib "$(BYTECCLIBS)"

partialclean::
	rm -f ocamlc.opt$(EXE)

# The native-code compiler compiled with itself

ocamlopt.opt$(EXE): \
                    compilerlibs/ocamlcommon.cmxa \
                    compilerlibs/ocamloptcomp.cmxa \
                    $(OPTSTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt.opt$(EXE)

# The predefined exceptions and primitives

runtime/primitives:
	$(MAKE) -C runtime primitives

lambda/runtimedef.ml: lambda/generate_runtimedef.sh runtime/caml/fail.h \
    runtime/primitives
	$^ > $@

partialclean::
	rm -f lambda/runtimedef.ml

beforedepend:: lambda/runtimedef.ml

# Choose the right machine-dependent files

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
	echo \# 1 \"$(ARCH)/emit.mlp\" > $@
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
	$(CAMLC) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f $(expunge)

# The runtime system for the bytecode compiler

$(SAK):
	$(MAKE) -C runtime sak$(EXE)

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
makeruntime:
	$(MAKE) -C runtime $(BOOT_FLEXLINK_CMD) all
runtime/libcamlrun.$(A): makeruntime ;
stdlib/libcamlrun.$(A): runtime/libcamlrun.$(A)
	cd stdlib; $(LN) ../runtime/libcamlrun.$(A) .
clean::
	$(MAKE) -C runtime clean
	rm -f stdlib/libcamlrun.a stdlib/libcamlrun.lib

otherlibs_all := bigarray dynlink \
  str systhreads unix win32unix
subdirs := debugger lex ocamldoc ocamltest stdlib tools \
  $(addprefix otherlibs/, $(otherlibs_all)) \

.PHONY: alldepend
alldepend: depend
	for dir in $(subdirs); do \
	  $(MAKE) -C $$dir depend || exit; \
	done

# The runtime system for the native-code compiler

.PHONY: runtimeopt
runtimeopt: stdlib/libasmrun.$(A)

.PHONY: makeruntimeopt
makeruntimeopt:
	$(MAKE) -C runtime $(BOOT_FLEXLINK_CMD) allopt
runtime/libasmrun.$(A): makeruntimeopt ;
stdlib/libasmrun.$(A): runtime/libasmrun.$(A)
	cp $< $@
clean::
	rm -f stdlib/libasmrun.a stdlib/libasmrun.lib

# The standard library

.PHONY: library
library: ocamlc
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) all

.PHONY: library-cross
library-cross:
	$(MAKE) -C stdlib \
	  $(BOOT_FLEXLINK_CMD) OCAMLRUN=../runtime/ocamlrun$(EXE) all

.PHONY: libraryopt
libraryopt:
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) allopt

partialclean::
	$(MAKE) -C stdlib clean

# The lexer and parser generators

.PHONY: ocamllex
ocamllex: ocamlyacc
	$(MAKE) -C lex all

.PHONY: ocamllex.opt
ocamllex.opt: ocamlopt
	$(MAKE) -C lex allopt

partialclean::
	$(MAKE) -C lex clean

.PHONY: ocamlyacc
ocamlyacc:
	$(MAKE) -C yacc $(BOOT_FLEXLINK_CMD) all

clean::
	$(MAKE) -C yacc clean

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
ifneq "$(NATIVE_COMPILER)" "true"
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
	$(MAKE) -C tools $(BOOT_FLEXLINK_CMD) checkstack$(EXE)
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

partialclean::
	$(MAKE) -C tools clean

## Test compilation of backend-specific parts

ARCH_SPECIFIC =\
  asmcomp/arch.ml asmcomp/proc.ml asmcomp/CSE.ml asmcomp/selection.ml \
  asmcomp/scheduling.ml asmcomp/reload.ml

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
	$(CAMLOPT_CMD) $(LINKFLAGS) -linkall -I toplevel/native -o $@ $^

toplevel/topdirs.cmx: toplevel/topdirs.ml
	$(CAMLOPT_CMD) $(COMPFLAGS) $(OPTCOMPFLAGS) -I toplevel/native -c $<

$(TOPLEVELINIT:.cmo=.cmx): $(TOPLEVELINIT:.cmo=.ml) \
     toplevel/native/topeval.cmx
	$(CAMLOPT_CMD) $(COMPFLAGS) $(OPTCOMPFLAGS) -I toplevel/native -c $<

$(TOPLEVELSTART:.cmo=.cmx): $(TOPLEVELSTART:.cmo=.ml) \
     toplevel/native/topmain.cmx
	$(CAMLOPT_CMD) $(COMPFLAGS) $(OPTCOMPFLAGS) -I toplevel/native -c $<

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
	$(CAMLC) $(COMPFLAGS) -c $< -I $(@D)

%.cmi: %.mli
	$(CAMLC) $(COMPFLAGS) -c $<

%.cmx: %.ml
	$(CAMLOPT) $(COMPFLAGS) $(OPTCOMPFLAGS) -c $< -I $(@D)

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
         driver toplevel toplevel/byte toplevel/native; \
	 do \
	   $(CAMLDEP) $(DEPFLAGS) -I $$d $(DEPINCLUDES) $$d/*.mli $$d/*.ml \
	   || exit; \
         done) > .depend

.PHONY: distclean
distclean: clean
	$(MAKE) -C manual distclean
	$(MAKE) -C runtime distclean
	$(MAKE) -C stdlib distclean
	rm -f boot/ocamlrun boot/ocamlrun.exe boot/camlheader \
	      boot/ocamlruns boot/ocamlruns.exe \
	      boot/flexlink.byte boot/flexlink.byte.exe \
	      boot/flexdll_*.o boot/flexdll_*.obj \
	      boot/*.cm* boot/libcamlrun.a boot/libcamlrun.lib boot/ocamlc.opt
	rm -f Makefile.config Makefile.build_config
	rm -rf autom4te.cache flexdll-sources
	rm -f config.log config.status libtool
	rm -f tools/eventlog_metadata
	rm -f tools/*.bak
	rm -f testsuite/_log*

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
