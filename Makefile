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

include Makefile.config
include Makefile.common

# For users who don't read the INSTALL file
.PHONY: defaultentry
defaultentry:
ifeq "$(UNIX_OR_WIN32)" "unix"
	@echo "Please refer to the installation instructions in file INSTALL."
	@echo "If you've just unpacked the distribution, something like"
	@echo "	./configure"
	@echo "	make world.opt"
	@echo "	make install"
	@echo "should work.  But see the file INSTALL for more details."
else
	@echo "Please refer to the instructions in file README.win32.adoc."
endif

MKDIR=mkdir -p
ifeq "$(UNIX_OR_WIN32)" "win32"
LN = cp
else
LN = ln -sf
endif

CAMLRUN ?= boot/ocamlrun
include stdlib/StdlibModules

CAMLC=$(CAMLRUN) boot/ocamlc -g -nostdlib -I boot -use-prims runtime/primitives
CAMLOPT=$(CAMLRUN) ./ocamlopt -g -nostdlib -I stdlib -I otherlibs/dynlink
ARCHES=amd64 i386 arm arm64 power s390x
INCLUDES=-I $(COMPLIBDIR) -I $(COMPLIBDIR_U) -I driver -I toplevel
COMPFLAGS=-strict-sequence -principal -absname -w +a-4-9-41-42-44-45-48-66 \
	  -warn-error A \
          -bin-annot -safe-string -strict-formats -no-alias-deps
LINKFLAGS=

ifeq "$(strip $(NATDYNLINKOPTS))" ""
OCAML_NATDYNLINKOPTS=
else
OCAML_NATDYNLINKOPTS = -ccopt "$(NATDYNLINKOPTS)"
endif

YACCFLAGS=-v --strict
CAMLLEX=$(CAMLRUN) boot/ocamllex
CAMLDEP=$(CAMLRUN) boot/ocamlc -depend
DEPFLAGS=-slash

OCAMLDOC_OPT=$(WITH_OCAMLDOC:=.opt)

include CompilerModules

BYTESTART=driver/main.cmo

OPTSTART=driver/optmain.cmo

TOPLEVELSTART=toplevel/topstart.cmo

OPTTOPLEVELSTART=toplevel/opttopstart.cmo

PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

TOPINCLUDES=$(addprefix -I otherlibs/,$(filter-out %threads,$(OTHERLIBRARIES)))
RUNTOP=./runtime/ocamlrun ./ocaml \
  -nostdlib -I stdlib \
  -noinit $(TOPFLAGS) $(TOPINCLUDES)
NATRUNTOP=./ocamlnat$(EXE) \
  -nostdlib -I stdlib \
  -noinit $(TOPFLAGS) $(TOPINCLUDES)
ifeq "$(UNIX_OR_WIN32)" "unix"
EXTRAPATH=
else
EXTRAPATH = PATH="otherlibs/win32unix:$(PATH)"
endif

BOOT_FLEXLINK_CMD=

ifeq "$(UNIX_OR_WIN32)" "win32"
FLEXDLL_SUBMODULE_PRESENT := $(wildcard flexdll/Makefile)
ifeq "$(FLEXDLL_SUBMODULE_PRESENT)" ""
  BOOT_FLEXLINK_CMD =
  FLEXDLL_DIR =
else
  BOOT_FLEXLINK_CMD = FLEXLINK_CMD="../boot/ocamlrun ../flexdll/flexlink.exe"
  FLEXDLL_DIR = $(if $(wildcard flexdll/flexdll_*.$(O)),+flexdll)
endif
else
  FLEXDLL_DIR =
endif

# The configuration file

utils/config.ml: utils/config.mlp Makefile.config utils/Makefile Makefile
	$(MAKE) -C utils config.ml

ifeq "$(UNIX_OR_WIN32)" "unix"
.PHONY: reconfigure
reconfigure:
	./configure $(CONFIGURE_ARGS)
endif

.PHONY: partialclean
partialclean::
	rm -f utils/config.ml

.PHONY: beforedepend
beforedepend:: utils/config.ml

# Start up the system from the distribution compiler
.PHONY: coldstart
coldstart:
	$(MAKE) -C runtime $(BOOT_FLEXLINK_CMD) all
	cp runtime/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) \
	  COMPILER="../boot/ocamlc -use-prims ../runtime/primitives" all
	cd stdlib; cp $(LIBFILES) ../boot
	cd boot; $(LN) ../runtime/libcamlrun.$(A) .

# Recompile the core system using the bootstrap compiler
.PHONY: coreall
coreall: runtime
	$(MAKE) ocamlc
	$(MAKE) ocamllex ocamltools library

# Build the core system: the minimum needed to make depend and bootstrap
.PHONY: core
core:
	$(MAKE) coldstart
	$(MAKE) coreall

# Check if fixpoint reached
.PHONY: compare
compare:
	@if $(CAMLRUN) tools/cmpbyt boot/ocamlc ocamlc \
         && $(CAMLRUN) tools/cmpbyt boot/ocamllex lex/ocamllex; \
	then echo "Fixpoint reached, bootstrap succeeded."; \
	else \
	  echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	  exit 1; \
	fi

# Promote a compiler

PROMOTE ?= cp

.PHONY: promote-common
promote-common:
	$(PROMOTE) ocamlc boot/ocamlc
	$(PROMOTE) lex/ocamllex boot/ocamllex
	cd stdlib; cp $(LIBFILES) ../boot

# Promote the newly compiled system to the rank of cross compiler
# (Runs on the old runtime, produces code for the new runtime)
.PHONY: promote-cross
promote-cross: promote-common

# Promote the newly compiled system to the rank of bootstrap compiler
# (Runs on the new runtime, produces code for the new runtime)
.PHONY: promote
promote: PROMOTE = $(CAMLRUN) tools/stripdebug
promote: promote-common
	cp runtime/ocamlrun$(EXE) boot/ocamlrun$(EXE)

# Compile the native-code compiler
.PHONY: opt-core
opt-core: runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) libraryopt

.PHONY: opt
opt:
	$(MAKE) runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) compilerlibs.opt
	$(MAKE) libraryopt
	$(MAKE) otherlibrariesopt ocamltoolsopt

# Native-code versions of the tools
.PHONY: opt.opt
opt.opt:
	$(MAKE) checkstack
	$(MAKE) runtime
	$(MAKE) core
	$(MAKE) ocaml
	$(MAKE) opt-core
	$(MAKE) ocamlc.opt
	$(MAKE) compilerlibs
	$(MAKE) compilerlibs.opt
	$(MAKE) compilerlibs.optopt
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) ocamltest
	$(MAKE) ocamlopt.opt
	$(MAKE) otherlibrariesopt
	$(MAKE) ocamllex.opt ocamltoolsopt ocamltoolsopt.opt $(OCAMLDOC_OPT) \
	  ocamltest.opt

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
	$(MAKE) CAMLRUN=runtime/ocamlrun promote
# Rebuild the core system
	$(MAKE) partialclean
	$(MAKE) core
# Check if fixpoint reached
	$(MAKE) compare

# Recompile the system using the bootstrap compiler

.PHONY: all
all: coreall
	$(MAKE) compilerlibs
	$(MAKE) ocaml
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) ocamltest

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
world.opt: coldstart
	$(MAKE) opt.opt

# FlexDLL sources missing error messages
# Different git mechanism displayed depending on whether this source tree came
# from a git clone or a source tarball.

flexdll/Makefile:
	@echo In order to bootstrap FlexDLL, you need to place the sources in
	@echo flexdll.
	@echo This can either be done by downloading a source tarball from
	@echo \  http://alain.frisch.fr/flexdll.html
	@if [ -d .git ]; then \
	  echo or by checking out the flexdll submodule with; \
	  echo \  git submodule update --init; \
	else \
	  echo or by cloning the git repository; \
	  echo \  git clone https://github.com/alainfrisch/flexdll.git; \
	fi
	@false

.PHONY: flexdll
flexdll: flexdll/Makefile flexlink
	$(MAKE) -C flexdll \
	     OCAML_CONFIG_FILE=../Makefile.config \
             MSVC_DETECT=0 CHAINS=$(FLEXDLL_CHAIN) NATDYNLINK=false support

# Bootstrapping flexlink - leaves a bytecode image of flexlink.exe in flexdll/
.PHONY: flexlink
flexlink: flexdll/Makefile
	$(MAKE) -C runtime BOOTSTRAPPING_FLEXLINK=yes ocamlrun$(EXE)
	cp runtime/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	$(MAKE) -C stdlib COMPILER=../boot/ocamlc stdlib.cma std_exit.cmo
	cd stdlib && cp stdlib.cma std_exit.cmo *.cmi ../boot
	$(MAKE) -C flexdll MSVC_DETECT=0 OCAML_CONFIG_FILE=../Makefile.config \
	  CHAINS=$(FLEXDLL_CHAIN) NATDYNLINK=false \
	  OCAMLOPT="../boot/ocamlrun ../boot/ocamlc -I ../boot" \
	  flexlink.exe
	$(MAKE) -C runtime clean
	$(MAKE) partialclean

.PHONY: flexlink.opt
flexlink.opt:
	cd flexdll && \
	mv flexlink.exe flexlink && \
	($(MAKE) OCAML_FLEXLINK="../boot/ocamlrun ./flexlink" MSVC_DETECT=0 \
	           OCAML_CONFIG_FILE=../Makefile.config \
	           OCAMLOPT="../ocamlopt.opt -I ../stdlib" flexlink.exe || \
	 (mv flexlink flexlink.exe && false)) && \
	mv flexlink.exe flexlink.opt && \
	mv flexlink flexlink.exe

INSTALL_COMPLIBDIR=$(DESTDIR)$(LIBDIR)/compiler-libs
INSTALL_FLEXDLLDIR=$(INSTALL_LIBDIR)/flexdll

.PHONY: install-flexdll
install-flexdll:
	cat stdlib/camlheader flexdll/flexlink.exe > \
	  "$(INSTALL_BINDIR)/flexlink.exe"
ifneq "$(filter-out mingw,$(TOOLCHAIN))" ""
	$(INSTALL_DATA) flexdll/default$(filter-out _i386,_$(ARCH)).manifest \
    "$(INSTALL_BINDIR)/"
endif
	if test -n "$(wildcard flexdll/flexdll_*.$(O))" ; then \
	  $(MKDIR) "$(INSTALL_FLEXDLLDIR)" ; \
	  $(INSTALL_DATA) flexdll/flexdll_*.$(O) "$(INSTALL_FLEXDLLDIR)" ; \
	fi

INSTALL_OCAMLCOMMONDIR=$(DESTDIR)$(LIBDIR)/ocamlcommon
INSTALL_OCAMLBYTECOMPDIR=$(DESTDIR)$(LIBDIR)/ocamlbytecomp
INSTALL_OCAMLOPTCOMPDIR=$(DESTDIR)$(LIBDIR)/ocamloptcomp
INSTALL_OCAMLTOPLEVELDIR=$(DESTDIR)$(LIBDIR)/ocamltoplevel

# Installation
.PHONY: install
install:
	$(MKDIR) "$(INSTALL_BINDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR)"
	$(MKDIR) "$(INSTALL_STUBLIBDIR)"
	$(MKDIR) "$(INSTALL_COMPLIBDIR)"
	$(MKDIR) "$(INSTALL_OCAMLCOMMONDIR)"
	$(MKDIR) "$(INSTALL_OCAMLBYTECOMPDIR)"
	$(MKDIR) "$(INSTALL_OCAMLTOPLEVELDIR)"
	$(INSTALL_DATA) \
	  VERSION \
	  "$(INSTALL_LIBDIR)"
	$(MAKE) -C runtime install
	$(INSTALL_PROG) ocaml "$(INSTALL_BINDIR)/ocaml$(EXE)"
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) ocamlc "$(INSTALL_BINDIR)/ocamlc.byte$(EXE)"
endif
	$(MAKE) -C stdlib install
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) lex/ocamllex "$(INSTALL_BINDIR)/ocamllex.byte$(EXE)"
endif
	$(INSTALL_PROG) yacc/ocamlyacc$(EXE) "$(INSTALL_BINDIR)/ocamlyacc$(EXE)"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/*.cmi $(COMPLIBDIR_U)/*.cmi \
	   "$(INSTALL_COMPLIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/*.cmt $(COMPLIBDIR)/*.cmti $(COMPLIBDIR)/*.mli \
	   $(COMPLIBDIR_U)/*.cmt $(COMPLIBDIR_U)/*.cmti $(COMPLIBDIR_U)/*.mli \
	   "$(INSTALL_COMPLIBDIR)"
endif
	$(INSTALL_DATA) \
	   $(COMPLIBDIR_U)/ocamlcommon.cma $(COMPLIBDIR_U)/ocamlbytecomp.cma \
	   $(COMPLIBDIR_U)/ocamltoplevel.cma $(BYTESTART) $(TOPLEVELSTART) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_common*.cmi "$(INSTALL_OCAMLCOMMONDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_bytecomp*.cmi "$(INSTALL_OCAMLBYTECOMPDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_toplevel*.cmi "$(INSTALL_OCAMLTOPLEVELDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_common*.cmt \
	  $(COMPLIBDIR)/ocaml_common*.cmti \
	  $(COMPLIBDIR)/ocaml_common.ml $(COMPLIBDIR)/ocaml_common__*.mli \
	  "$(INSTALL_OCAMLCOMMONDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_bytecomp*.cmt \
	  $(COMPLIBDIR)/ocaml_bytecomp*.cmti \
	  $(COMPLIBDIR)/ocaml_bytecomp.ml $(COMPLIBDIR)/ocaml_bytecomp__*.mli \
	  "$(INSTALL_OCAMLBYTECOMPDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_toplevel*.cmt \
	  $(COMPLIBDIR)/ocaml_toplevel*.cmti \
	  $(COMPLIBDIR)/ocaml_toplevel.ml $(COMPLIBDIR)/ocaml_toplevel__*.mli \
	  "$(INSTALL_OCAMLTOPLEVELDIR)"
endif
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocamlcommon.cma "$(INSTALL_OCAMLCOMMONDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocamlbytecomp.cma "$(INSTALL_OCAMLBYTECOMPDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocamltoplevel.cma "$(INSTALL_OCAMLTOPLEVELDIR)"
	$(INSTALL_PROG) expunge "$(INSTALL_LIBDIR)/expunge$(EXE)"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR_U)/topdirs.cmi \
	   "$(INSTALL_LIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/ocaml_toplevel__topdirs.cmti \
	   $(COMPLIBDIR)/ocaml_toplevel__topdirs.mli \
	   $(COMPLIBDIR_U)/topdirs.cmt \
           $(COMPLIBDIR_U)/topdirs.ml \
	   "$(INSTALL_LIBDIR)"
endif
	$(MAKE) -C tools install
ifeq "$(UNIX_OR_WIN32)" "unix" # Install manual pages only on Unix
	$(MKDIR) "$(INSTALL_MANDIR)/man$(PROGRAMS_MAN_SECTION)"
	-$(MAKE) -C man install
endif
	for i in $(OTHERLIBRARIES); do \
	  $(MAKE) -C otherlibs/$$i install || exit $$?; \
	done
# Transitional: findlib 1.7.3 is confused if leftover num.cm? files remain
# from an previous installation of OCaml before otherlibs/num was removed.
	rm -f "$(INSTALL_LIBDIR)"/num.cm?
# End transitional
	if test -n "$(WITH_OCAMLDOC)"; then \
	  $(MAKE) -C ocamldoc install; \
	fi
	if test -n "$(WITH_DEBUGGER)"; then \
	  $(MAKE) -C debugger install; \
	fi
ifeq "$(UNIX_OR_WIN32)" "win32"
	if test -n "$(FLEXDLL_SUBMODULE_PRESENT)"; then \
	  $(MAKE) install-flexdll; \
	fi
endif
	$(INSTALL_DATA) Makefile.config "$(INSTALL_LIBDIR)/Makefile.config"
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	if test -f ocamlopt; then $(MAKE) installopt; else \
	   cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.byte$(EXE) ocamlc$(EXE); \
	   $(LN) ocamllex.byte$(EXE) ocamllex$(EXE); \
	fi
else
	if test -f ocamlopt; then $(MAKE) installopt; fi
endif

# Installation of the native-code compiler
.PHONY: installopt
installopt:
	$(MKDIR) "$(INSTALL_OCAMLOPTCOMPDIR)"
	$(MAKE) -C runtime installopt
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) ocamlopt "$(INSTALL_BINDIR)/ocamlopt.byte$(EXE)"
endif
	$(MAKE) -C stdlib installopt
	$(INSTALL_DATA) \
	    $(COMPLIBDIR)/ocaml_*__*.cmi $(COMPLIBDIR_U)/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	    $(COMPLIBDIR)/*.cmt $(COMPLIBDIR)/*.cmti $(COMPLIBDIR)/*.mli \
	    $(COMPLIBDIR_U)/*.cmt $(COMPLIBDIR_U)/*.cmti $(COMPLIBDIR_U)/*.mli \
	    $(COMPLIBDIR_U)/*.ml \
	    "$(INSTALL_COMPLIBDIR)"
endif
	$(INSTALL_DATA) \
	    $(COMPLIBDIR_U)/ocamloptcomp.cma $(OPTSTART) \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    $(COMPLIBDIR)/ocaml_optcomp*.cmi \
	    "$(INSTALL_OCAMLOPTCOMPDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	    $(COMPLIBDIR)/ocaml_optcomp*.cmt \
	    $(COMPLIBDIR)/ocaml_optcomp*.cmti \
	    $(COMPLIBDIR)/ocaml_optcomp.ml \
	    $(COMPLIBDIR)/ocaml_optcomp__*.mli \
	    "$(INSTALL_OCAMLOPTCOMPDIR)"
endif
	$(INSTALL_DATA) \
	    $(COMPLIBDIR)/ocamloptcomp.cma $(OPTSTART) \
	    "$(INSTALL_OCAMLOPTCOMPDIR)"
	if test -n "$(WITH_OCAMLDOC)"; then \
	  $(MAKE) -C ocamldoc installopt; \
	fi
	for i in $(OTHERLIBRARIES); do \
	  $(MAKE) -C otherlibs/$$i installopt || exit $$?; \
	done
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	if test -f ocamlopt.opt ; then $(MAKE) installoptopt; else \
	   cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.byte$(EXE) ocamlc$(EXE); \
	   $(LN) ocamlopt.byte$(EXE) ocamlopt$(EXE); \
	   $(LN) ocamllex.byte$(EXE) ocamllex$(EXE); \
	fi
else
	if test -f ocamlopt.opt ; then $(MAKE) installoptopt; fi
endif
	$(MAKE) -C tools installopt
	if test -f ocamlopt.opt -a -f flexdll/flexlink.opt ; then \
	  $(INSTALL_PROG) \
	    flexdll/flexlink.opt "$(INSTALL_BINDIR)/flexlink$(EXE)" ; \
	fi

.PHONY: installoptopt
installoptopt:
	$(INSTALL_PROG) ocamlc.opt "$(INSTALL_BINDIR)/ocamlc.opt$(EXE)"
	$(INSTALL_PROG) ocamlopt.opt "$(INSTALL_BINDIR)/ocamlopt.opt$(EXE)"
	$(INSTALL_PROG) \
	  lex/ocamllex.opt "$(INSTALL_BINDIR)/ocamllex.opt$(EXE)"
	cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.opt$(EXE) ocamlc$(EXE); \
	   $(LN) ocamlopt.opt$(EXE) ocamlopt$(EXE); \
	   $(LN) ocamllex.opt$(EXE) ocamllex$(EXE)
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/ocaml_*__*.cmx $(COMPLIBDIR_U)/*.cmx \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
           $(COMPLIBDIR_U)/ocamlcommon.cmxa $(COMPLIBDIR_U)/ocamlcommon.$(A) \
	   $(COMPLIBDIR_U)/ocamlbytecomp.cmxa \
	   $(COMPLIBDIR_U)/ocamlbytecomp.$(A) \
	   $(COMPLIBDIR_U)/ocamloptcomp.cmxa $(COMPLIBDIR_U)/ocamloptcomp.$(A) \
	   $(BYTESTART:.cmo=.cmx) $(BYTESTART:.cmo=.$(O)) \
	   $(OPTSTART:.cmo=.cmx) $(OPTSTART:.cmo=.$(O)) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/ocaml_common*.cmx \
	   "$(INSTALL_OCAMLCOMMONDIR)"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/ocaml_bytecomp*.cmx \
	   "$(INSTALL_OCAMLBYTECOMPDIR)"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/ocaml_optcomp*.cmx \
	   "$(INSTALL_OCAMLOPTCOMPDIR)"
	$(INSTALL_DATA) \
           $(COMPLIBDIR)/ocamlcommon.cmxa \
	   $(COMPLIBDIR)/ocamlcommon.$(A) \
	   "$(INSTALL_OCAMLCOMMONDIR)"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/ocamlbytecomp.cmxa \
	   $(COMPLIBDIR)/ocamlbytecomp.$(A) \
	   $(BYTESTART:.cmo=.cmx) $(BYTESTART:.cmo=.$(O)) \
	   "$(INSTALL_OCAMLBYTECOMPDIR)"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/ocamloptcomp.cmxa \
	   $(COMPLIBDIR)/ocamloptcomp.$(A) \
	   $(OPTSTART:.cmo=.cmx) $(OPTSTART:.cmo=.$(O)) \
	   "$(INSTALL_OCAMLOPTCOMPDIR)"
	if test -f ocamlnat$(EXE) ; then \
	  $(INSTALL_PROG) \
	    ocamlnat$(EXE) "$(INSTALL_BINDIR)/ocamlnat$(EXE)"; \
	  $(INSTALL_DATA) \
	     $(COMPLIBDIR_U)/opttopdirs.cmi \
	     "$(INSTALL_LIBDIR)"; \
	  $(INSTALL_DATA) \
	     $(COMPLIBDIR_U)/ocamlopttoplevel.cmxa \
	     $(COMPLIBDIR_U)/ocamlopttoplevel.$(A) \
	     $(OPTTOPLEVELSTART:.cmo=.cmx) $(OPTTOPLEVELSTART:.cmo=.$(O)) \
	     "$(INSTALL_COMPLIBDIR)"; \
	fi
	cd "$(INSTALL_COMPLIBDIR)" && \
	   $(RANLIB) ocamlcommon.$(A) ocamlbytecomp.$(A) ocamloptcomp.$(A)
	cd "$(INSTALL_OCAMLCOMMONDIR)" && \
	   $(RANLIB) ocamlcommon.$(A)
	cd "$(INSTALL_OCAMLBYTECOMPDIR)" && \
	   $(RANLIB) ocamlbytecomp.$(A)
	cd "$(INSTALL_OCAMLOPTCOMPDIR)" && \
	   $(RANLIB) ocamloptcomp.$(A)

# Installation of the *.ml sources of compiler-libs
.PHONY: install-compiler-sources
install-compiler-sources:
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   utils/*.ml parsing/*.ml typing/*.ml bytecomp/*.ml driver/*.ml \
	   toplevel/*.ml middle_end/*.ml middle_end/base_types/*.ml \
	   asmcomp/*.ml \
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

# The clean target
clean:: partialclean

# Prefixed compiler-libs

$(COMPLIBDIR)/%.cmi: $(COMPLIBDIR)/%.mli
	$(CAMLC) $(COMPFLAGS) -I $(COMPLIBDIR) -c $<

$(COMPLIBDIR)/%.cmo: $(COMPLIBDIR)/%.ml
	$(CAMLC) $(COMPFLAGS) -I $(COMPLIBDIR) -c $<

$(COMPLIBDIR)/%.cmx: $(COMPLIBDIR)/%.ml ocamlopt
	$(CAMLOPT) $(COMPFLAGS) -I $(COMPLIBDIR) -c $<

$(COMPLIBDIR)/ocaml_common__compdynlink.cmo: \
	$(COMPLIBDIR)/ocaml_common__compdynlink.mlbyte
	$(CAMLC) $(COMPFLAGS) -I $(COMPLIBDIR) -c -impl $<

$(COMPLIBDIR)/ocaml_common__compdynlink.cmx: \
	$(COMPLIBDIR)/ocaml_common__compdynlink.mlopt \
	$(COMPLIBDIR)/ocaml_optcomp__cmx_format.cmi ocamlopt
	$(CAMLOPT) $(COMPFLAGS) -I $(COMPLIBDIR) -c -impl $<

tools/gen_prefix: tools/gen_prefix.ml
	$(CAMLC) -o $@ $<

define copy_file_with_prefix
$(COMPLIBDIR)/ocaml_$(1)__$(notdir $(2)): $(2)
	(for d in $(3); do echo "open! $$$${d}"; done; cat $$<) > $$@

beforedepend:: $(COMPLIBDIR)/ocaml_$(1)__$(notdir $(2))
endef

define copy_files_with_prefix
$(foreach f,\
  $(addsuffix .ml,$(filter-out $(MLI_ONLY),$(2))) \
  $(addsuffix .mli,$(filter-out $(ML_ONLY),$(2))),\
  $(eval $(call copy_file_with_prefix,$(1),$(f),$(3))))
endef

$(call copy_files_with_prefix,common,\
$(filter-out driver/compdynlink,$(COMMON)),Ocaml_common)

$(COMPLIBDIR)/ocaml_common__compdynlink.mlbyte: driver/compdynlink.mlbyte
	(echo 'open! Ocaml_common'; cat $<) > $@

$(COMPLIBDIR)/ocaml_common__compdynlink.mlopt: driver/compdynlink.mlopt
	(echo 'open! Ocaml_common'; \
	 echo 'module Cmx_format = Ocaml_optcomp__cmx_format'; \
	 cat $<) > $@

$(COMPLIBDIR)/ocaml_common__compdynlink.mli: driver/compdynlink.mli
	(echo 'open! Ocaml_common'; cat $<) > $@

beforedepend:: $(COMPLIBDIR)/ocaml_common__compdynlink.mlbyte \
	$(COMPLIBDIR)/ocaml_common__compdynlink.mlopt \
	$(COMPLIBDIR)/ocaml_common__compdynlink.mli

$(call copy_files_with_prefix,bytecomp,$(BYTECOMP),Ocaml_common Ocaml_bytecomp)
$(call copy_files_with_prefix,optcomp,$(OPTCOMP),Ocaml_common Ocaml_optcomp)
$(call copy_files_with_prefix,toplevel,$(TOPLEVEL),\
Ocaml_common Ocaml_bytecomp Ocaml_toplevel)

partialclean::
	rm -f $(COMPLIBDIR_U)/*.ml $(COMPLIBDIR_U)/*.mli

$(COMPLIBDIR_U)/%.cmi: $(COMPLIBDIR_U)/%.mli
	$(CAMLC) $(COMPFLAGS) -I $(COMPLIBDIR) -c $<

$(COMPLIBDIR_U)/%.cmo: $(COMPLIBDIR_U)/%.ml
	$(CAMLC) $(COMPFLAGS) -I $(COMPLIBDIR) -c $<

$(COMPLIBDIR_U)/%.cmx: $(COMPLIBDIR_U)/%.ml ocamlopt
	$(CAMLOPT) $(COMPFLAGS) -I $(COMPLIBDIR) -c $<

.PHONY: compilerlibs
compilerlibs: \
    $(COMPLIBDIR)/ocamlcommon.cma \
    $(COMPLIBDIR)/ocamlbytecomp.cma \
    $(COMPLIBDIR)/ocamltoplevel.cma

.PHONY: compilerlibs.opt
compilerlibs.opt: \
    $(COMPLIBDIR)/ocamloptcomp.cma

.PHONY: compilerlibs.optopt
compilerlibs.optopt: \
    $(COMPLIBDIR)/ocamlcommon.cmxa \
    $(COMPLIBDIR)/ocamlbytecomp.cmxa \
    $(COMPLIBDIR)/ocamloptcomp.cmxa \
    $(COMPLIBDIR)/ocamlopttoplevel.cmxa

# Shared parts of the system

$(COMPLIBDIR)/ocaml_common.ml: tools/gen_prefix CompilerModules
	$(CAMLRUN) $< -prefix ocaml_common $(COMMON) > $@
$(COMPLIBDIR)/ocaml_common.cmo: $(COMPLIBDIR)/ocaml_common.ml
	$(CAMLC) $(COMPFLAGS) -w -49 -c $<
$(COMPLIBDIR)/ocamlcommon.cma: $(COMMON_CMO)
	$(CAMLC) -a -linkall -o $@ $^

# Legacy unprefixed ocamlcommon

$(addprefix $(COMPLIBDIR_U)/,$(COMMON_ML:=.ml) $(COMMON_MLI_ONLY:=.mli)): \
	$(COMPLIBDIR_U)/common
$(COMPLIBDIR_U)/ocamlcommon.cma: $(COMMON_CMI_U) $(COMMON_CMO_U)
	$(CAMLC) -a -linkall -o $@ $(filter %.cmo,$^)
$(COMPLIBDIR_U)/common: tools/gen_prefix CompilerModules
	$(CAMLRUN) $< $(COMMON_ML) -mli $(COMMON_MLI_ONLY) \
	  -prefix ocaml_common -unprefix $(COMPLIBDIR_U)
	touch $@

partialclean::
	rm -f $(COMPLIBDIR)/ocaml_common.ml $(COMPLIBDIR)/ocamlcommon.cma
	rm -f $(COMPLIBDIR_U)/ocamlcommon.cma $(COMPLIBDIR_U)/common

beforedepend:: $(COMPLIBDIR)/ocaml_common.ml $(COMPLIBDIR_U)/common

# The bytecode compiler

$(COMPLIBDIR)/ocaml_bytecomp.ml: tools/gen_prefix CompilerModules
	$(CAMLRUN) $< -prefix ocaml_bytecomp $(BYTECOMP) > $@
$(COMPLIBDIR)/ocaml_bytecomp.cmo: $(COMPLIBDIR)/ocaml_bytecomp.ml
	$(CAMLC) $(COMPFLAGS) -w -49 -c $<
$(COMPLIBDIR)/ocamlbytecomp.cma: $(BYTECOMP_CMO)
	$(CAMLC) -a -o $@ $^

# Legacy unprefixed ocamlbytecomp

$(addprefix $(COMPLIBDIR_U)/,$(BYTECOMP_ML:=.ml) $(BYTECOMP_MLI_ONLY:=.mli)): \
	$(COMPLIBDIR_U)/bytecomp
$(COMPLIBDIR_U)/ocamlbytecomp.cma: $(BYTECOMP_CMI_U) $(BYTECOMP_CMO_U)
	$(CAMLC) -a -o $@ $(filter %.cmo,$^)
$(COMPLIBDIR_U)/bytecomp: tools/gen_prefix CompilerModules
	$(CAMLRUN) $< $(BYTECOMP_ML) -mli $(BYTECOMP_MLI_ONLY) \
	  -prefix ocaml_bytecomp -unprefix $(COMPLIBDIR_U)
	touch $@

partialclean::
	rm -f $(COMPLIBDIR)/ocaml_bytecomp.ml $(COMPLIBDIR)/ocamlbytecomp.cma
	rm -f $(COMPLIBDIR_U)/ocamlbytecomp.cma $(COMPLIBDIR_U)/bytecomp

beforedepend:: $(COMPLIBDIR)/ocaml_bytecomp.ml $(COMPLIBDIR_U)/bytecomp

ocamlc: $(COMPLIBDIR_U)/ocamlcommon.cma $(COMPLIBDIR_U)/ocamlbytecomp.cma \
	$(BYTESTART)
	$(CAMLC) $(LINKFLAGS) -compat-32 -o $@ $^

partialclean::
	rm -rf ocamlc

# The native-code compiler

$(COMPLIBDIR)/ocaml_optcomp.ml: tools/gen_prefix CompilerModules
	$(CAMLRUN) $< -prefix ocaml_optcomp $(OPTCOMP) > $@
$(COMPLIBDIR)/ocaml_optcomp.cmo: $(COMPLIBDIR)/ocaml_optcomp.ml
	$(CAMLC) $(COMPFLAGS) -w -49 -c $<
$(COMPLIBDIR)/ocamloptcomp.cma: $(OPTCOMP_CMO)
	$(CAMLC) -a -o $@ $^

# Legacy unprefixed ocamloptcomp

$(addprefix $(COMPLIBDIR_U)/,$(OPTCOMP_ML:=.ml) $(OPTCOMP_MLI_ONLY:=.mli)): \
	$(COMPLIBDIR_U)/optcomp
$(COMPLIBDIR_U)/ocamloptcomp.cma: $(OPTCOMP_CMI_U) $(OPTCOMP_CMO_U)
	$(CAMLC) -a -o $@ $(filter %.cmo,$^)
$(COMPLIBDIR_U)/optcomp: tools/gen_prefix CompilerModules
	$(CAMLRUN) $< $(OPTCOMP_ML) -mli $(OPTCOMP_MLI_ONLY) \
	  -prefix ocaml_optcomp -unprefix $(COMPLIBDIR_U)
	touch $@

partialclean::
	rm -f $(COMPLIBDIR)/ocaml_optcomp.ml $(COMPLIBDIR)/ocamloptcomp.cma
	rm -f $(COMPLIBDIR_U)/ocamloptcomp.cma $(COMPLIBDIR_U)/optcomp

beforedepend:: $(COMPLIBDIR)/ocaml_optcomp.ml $(COMPLIBDIR_U)/optcomp

ocamlopt: $(COMPLIBDIR_U)/ocamlcommon.cma $(COMPLIBDIR_U)/ocamloptcomp.cma \
          $(OPTSTART)
	$(CAMLC) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt

# The toplevel

$(COMPLIBDIR)/ocaml_toplevel.ml: tools/gen_prefix CompilerModules
	$(CAMLRUN) $< -prefix ocaml_toplevel $(TOPLEVEL) > $@
$(COMPLIBDIR)/ocaml_toplevel.cmo: $(COMPLIBDIR)/ocaml_toplevel.ml
	$(CAMLC) $(COMPFLAGS) -w -49 -c $<
$(COMPLIBDIR)/ocaml_toplevel.cmx: $(COMPLIBDIR)/ocaml_toplevel.ml
	$(CAMLOPT) $(COMPFLAGS) -w -49 -c $<
$(COMPLIBDIR)/ocamltoplevel.cma: $(TOPLEVEL_CMO)
	$(CAMLC) -a -o $@ $^

# Legacy unprefixed ocamltoplevel

$(addprefix $(COMPLIBDIR_U)/,$(TOPLEVEL_ML:=.ml) $(TOPLEVEL_MLI_ONLY:=.mli)): \
	$(COMPLIBDIR_U)/toplevel
$(COMPLIBDIR_U)/ocamltoplevel.cma: $(TOPLEVEL_CMI_U) $(TOPLEVEL_CMO_U)
	$(CAMLC) -a -o $@ $(filter %.cmo,$^)
$(COMPLIBDIR_U)/toplevel: tools/gen_prefix CompilerModules
	$(CAMLRUN) $< $(TOPLEVEL_ML) -mli $(TOPLEVEL_MLI_ONLY) \
	  -prefix ocaml_toplevel -unprefix $(COMPLIBDIR_U)
	touch $@

partialclean::
	rm -f $(COMPLIBDIR)/ocaml_toplevel.ml $(COMPLIBDIR)/ocamltoplevel.cma
	rm -f $(COMPLIBDIR_U)/ocamltoplevel.cma $(COMPLIBDIR_U)/toplevel

beforedepend:: $(COMPLIBDIR)/ocaml_toplevel.ml $(COMPLIBDIR_U)/toplevel

ocaml_dependencies := \
  $(COMPLIBDIR_U)/ocamlcommon.cma \
  $(COMPLIBDIR_U)/ocamlbytecomp.cma \
  $(COMPLIBDIR_U)/ocamltoplevel.cma $(TOPLEVELSTART)

.INTERMEDIATE: ocaml.tmp
ocaml.tmp: $(ocaml_dependencies)
	$(CAMLC) $(LINKFLAGS) -linkall -o $@ $^

ocaml: expunge ocaml.tmp
	- $(CAMLRUN) $^ $@ $(PERVASIVES)

partialclean::
	rm -f ocaml

.PHONY: runtop
runtop:
	$(MAKE) coldstart
	$(MAKE) ocamlc
	$(MAKE) otherlibraries
	$(MAKE) ocaml
	@rlwrap --help 2>/dev/null && $(EXTRAPATH) rlwrap $(RUNTOP) ||\
	  $(EXTRAPATH) $(RUNTOP)

.PHONY: natruntop
natruntop:
	$(MAKE) core
	$(MAKE) opt
	$(MAKE) ocamlnat
	@rlwrap --help 2>/dev/null && $(EXTRAPATH) rlwrap $(NATRUNTOP) ||\
	  $(EXTRAPATH) $(NATRUNTOP)

# Native dynlink

otherlibs/dynlink/dynlink.cmxa: otherlibs/dynlink/natdynlink.ml
	$(MAKE) -C otherlibs/dynlink allopt

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) $<

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# Shared parts of the system compiled with the native-code compiler

$(COMPLIBDIR)/ocaml_common.cmx: $(COMPLIBDIR)/ocaml_common.ml
	$(CAMLOPT) $(COMPFLAGS) -w -49 -c $<
$(COMPLIBDIR)/ocamlcommon.cmxa: $(COMMON_CMO:.cmo=.cmx)
	$(CAMLOPT) -a -linkall -o $@ $^
$(COMPLIBDIR_U)/ocamlcommon.cmxa: $(COMPLIBDIR_U)/common \
    $(COMMON_CMI_U) $(COMMON_CMO_U:.cmo=.cmx)
	$(CAMLOPT) -a -linkall -o $@ $(filter %.cmx,$^)
partialclean::
	rm -f $(COMPLIBDIR)/ocamlcommon.cmxa $(COMPLIBDIR)/ocamcommon.$(A)
	rm -f $(COMPLIBDIR_U)/ocamlcommon.cmxa $(COMPLIBDIR_U)/ocamlcommon.$(A)

# The bytecode compiler compiled with the native-code compiler

$(COMPLIBDIR)/ocaml_bytecomp.cmx: $(COMPLIBDIR)/ocaml_bytecomp.ml
	$(CAMLOPT) $(COMPFLAGS) -w -49 -c $<
$(COMPLIBDIR)/ocamlbytecomp.cmxa: $(BYTECOMP_CMO:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $^
$(COMPLIBDIR_U)/ocamlbytecomp.cmxa: $(COMPLIBDIR_U)/bytecomp \
    $(BYTECOMP_CMI_U) $(BYTECOMP_CMO_U:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(filter %.cmx,$^)
partialclean::
	rm -f $(COMPLIBDIR)/ocamlbytecomp.cmxa $(COMPLIBDIR)/ocamlbytecomp.$(A)
	rm -f $(COMPLIBDIR_U)/ocamlbytecomp.cmxa \
	  $(COMPLIBDIR_U)/ocamlbytecomp.$(A)

ocamlc.opt: $(COMPLIBDIR_U)/ocamlcommon.cmxa \
	    $(COMPLIBDIR_U)/ocamlbytecomp.cmxa \
            $(BYTESTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -o $@ $^ -cclib "$(BYTECCLIBS)"

partialclean::
	rm -f ocamlc.opt

# The native-code compiler compiled with itself

$(COMPLIBDIR)/ocaml_optcomp.cmx: $(COMPLIBDIR)/ocaml_optcomp.ml
	$(CAMLOPT) $(COMPFLAGS) -w -49 -c $<
$(COMPLIBDIR)/ocamloptcomp.cmxa: $(OPTCOMP_CMO:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $^
$(COMPLIBDIR_U)/ocamloptcomp.cmxa: $(OPTCOMP_CMI_U) $(OPTCOMP_CMO_U:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(filter %.cmx,$^)
partialclean::
	rm -f $(COMPLIBDIR)/ocamloptcomp.cmxa $(COMPLIBDIR)/ocamloptcomp.$(A)
	rm -f $(COMPLIBDIR_U)/ocamloptcomp.cmxa \
	  $(COMPLIBDIR_U)/ocamloptcomp.$(A)

ocamlopt.opt: $(COMPLIBDIR_U)/ocamlcommon.cmxa \
	      $(COMPLIBDIR_U)/ocamloptcomp.cmxa \
              $(OPTSTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt.opt

# The predefined exceptions and primitives

runtime/primitives:
	$(MAKE) -C runtime primitives

bytecomp/runtimedef.ml: bytecomp/generate_runtimedef.sh runtime/caml/fail.h \
    runtime/primitives
	$^ > $@

partialclean::
	rm -f bytecomp/runtimedef.ml

beforedepend:: bytecomp/runtimedef.ml

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

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp tools/cvt_emit
	echo \# 1 \"$(ARCH)/emit.mlp\" > $@
	$(CAMLRUN) tools/cvt_emit < $< >> $@ \
	|| { rm -f $@; exit 2; }

partialclean::
	rm -f asmcomp/emit.ml

beforedepend:: asmcomp/emit.ml

tools/cvt_emit: tools/cvt_emit.mll
	$(MAKE) -C tools cvt_emit

# The "expunge" utility

expunge: $(COMPLIBDIR_U)/ocamlcommon.cma $(COMPLIBDIR_U)/ocamlbytecomp.cma \
         toplevel/expunge.cmo
	$(CAMLC) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f expunge

# The runtime system for the bytecode compiler

.PHONY: runtime
runtime: stdlib/libcamlrun.$(A)

.PHONY: makeruntime
makeruntime:
	$(MAKE) -C runtime $(BOOT_FLEXLINK_CMD) all
runtime/libcamlrun.$(A): makeruntime ;
stdlib/libcamlrun.$(A): runtime/libcamlrun.$(A)
	cd stdlib; $(LN) ../runtime/libcamlrun.$(A) .
clean::
	$(MAKE) -C runtime clean
	rm -f stdlib/libcamlrun.$(A)

otherlibs_all := bigarray dynlink graph raw_spacetime_lib \
  str systhreads threads unix win32graph win32unix
subdirs := debugger lex ocamldoc ocamltest runtime stdlib tools \
  $(addprefix otherlibs/, $(otherlibs_all)) \

.PHONY: alldepend
ifeq "$(TOOLCHAIN)" "msvc"
alldepend:
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
alldepend: depend
	for dir in $(subdirs); do \
	  $(MAKE) -C $$dir depend || exit; \
	done
endif

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
	rm -f stdlib/libasmrun.$(A)

# The standard library

.PHONY: library
library: ocamlc
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) all

.PHONY: library-cross
library-cross:
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) CAMLRUN=../runtime/ocamlrun all

.PHONY: libraryopt
libraryopt:
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) allopt

partialclean::
	$(MAKE) -C stdlib clean

# The lexer and parser generators

.PHONY: ocamllex
ocamllex: ocamlyacc ocamlc
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
	cp $< $@

# Copy parsing/parser.ml from boot/

parsing/parser.ml: boot/menhir/parser.ml parsing/parser.mly \
  tools/check-parser-uptodate-or-warn.sh
	@tools/check-parser-uptodate-or-warn.sh
	cat $< | sed "s/MenhirLib/CamlinternalMenhirLib/g" > $@
parsing/parser.mli: boot/menhir/parser.mli
	cat $< | sed "s/MenhirLib/CamlinternalMenhirLib/g" > $@

$(COMPLIBDIR)/ocaml_common__parser.mly: parsing/parser.mly
	cp $< $@

partialclean:: partialclean-menhir


# OCamldoc

.PHONY: ocamldoc
ocamldoc: ocamlc ocamlyacc ocamllex otherlibraries
	$(MAKE) -C ocamldoc all

.PHONY: ocamldoc.opt
ocamldoc.opt: ocamlc.opt ocamlyacc ocamllex
	$(MAKE) -C ocamldoc opt.opt

# OCamltest
ocamltest: ocamlc ocamlyacc ocamllex
	$(MAKE) -C ocamltest

ocamltest.opt: ocamlc.opt ocamlyacc ocamllex
	$(MAKE) -C ocamltest ocamltest.opt$(EXE)

partialclean::
	$(MAKE) -C ocamltest clean

# Documentation

.PHONY: html_doc
html_doc: ocamldoc
	$(MAKE) -C ocamldoc $@
	@echo "documentation is in ./ocamldoc/stdlib_html/"

partialclean::
	$(MAKE) -C ocamldoc clean

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

# Check that the stack limit is reasonable (Unix-only)
.PHONY: checkstack
checkstack:
ifeq "$(UNIX_OR_WIN32)" "unix"
	if $(MKEXE) $(OUTPUTEXE)tools/checkstack$(EXE) tools/checkstack.c; \
	  then tools/checkstack$(EXE); \
	fi
	rm -f tools/checkstack$(EXE)
else
	@
endif

# Lint @since and @deprecated annotations

VERSIONS=$(shell git tag|grep '^[0-9]*.[0-9]*.[0-9]*$$'|grep -v '^[12].')
.PHONY: lintapidiff
lintapidiff:
	$(MAKE) -C tools lintapidiff.opt
	git ls-files -- 'otherlibs/*/*.mli' 'stdlib/*.mli' |\
	    grep -Ev internal\|obj\|spacetime\|stdLabels\|moreLabels |\
	    tools/lintapidiff.opt $(VERSIONS)

# The middle end (whose .cma library is currently only used for linking
# the "ocamlobjinfo" program, since we cannot depend on the whole native code
# compiler for "make world" and the list of dependencies for
# asmcomp/export_info.cmo is long).

$(COMPLIBDIR_U)/ocamlmiddleend.cma: \
	$(MIDDLE_END_CMI_U) $(MIDDLE_END_CMO_U)
	$(CAMLC) -a -o $@ $(filter %.cmo,$^)
$(COMPLIBDIR_U)/ocamlmiddleend.cmxa: \
	$(MIDDLE_END_CMI_U) $(MIDDLE_END_CMO_U:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(filter %.cmx,$^)

partialclean::
	rm -f $(COMPLIBDIR_U)/ocamlmiddleend.cma \
	      $(COMPLIBDIR_U)/ocamlmiddleend.cmxa \
	      $(COMPLIBDIR_U)/ocamlmiddleend.$(A)

# Tools

.PHONY: ocamltools
ocamltools: ocamlc ocamllex \
            $(COMPLIBDIR_U)/cmx_format.cmi \
            $(COMPLIBDIR)/ocaml_optcomp__printclambda.cmo \
            $(COMPLIBDIR_U)/printclambda.cmo \
            $(COMPLIBDIR_U)/ocamlmiddleend.cma \
            $(COMPLIBDIR)/ocaml_optcomp__backend_var.cmo \
            $(COMPLIBDIR_U)/backend_var.cmo \
            $(COMPLIBDIR)/ocaml_optcomp__export_info.cmo \
            $(COMPLIBDIR_U)/export_info.cmo
	$(MAKE) -C tools all

.PHONY: ocamltoolsopt
ocamltoolsopt: ocamlopt
	$(MAKE) -C tools opt

.PHONY: ocamltoolsopt.opt
ocamltoolsopt.opt: ocamlc.opt ocamllex.opt \
                   $(COMPLIBDIR_U)/cmx_format.cmi \
                   $(COMPLIBDIR)/ocaml_optcomp__printclambda.cmx \
                   $(COMPLIBDIR_U)/printclambda.cmx \
                   $(COMPLIBDIR_U)/ocamlmiddleend.cmxa \
                   $(COMPLIBDIR)/ocaml_optcomp__backend_var.cmx \
                   $(COMPLIBDIR_U)/backend_var.cmx \
                   $(COMPLIBDIR)/ocaml_optcomp__export_info.cmx \
                   $(COMPLIBDIR_U)/export_info.cmx
	$(MAKE) -C tools opt.opt

partialclean::
	$(MAKE) -C tools clean

## Test compilation of backend-specific parts

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

# Compiler Plugins

DYNLINK_DIR=otherlibs/dynlink

driver/compdynlink.mlbyte: $(DYNLINK_DIR)/dynlink.ml driver/compdynlink.mli \
    driver/compify_dynlink.sh
	driver/compify_dynlink.sh $< $@

driver/compdynlink_common.ml: $(DYNLINK_DIR)/dynlink_common.ml \
    driver/compify_dynlink.sh
	driver/compify_dynlink.sh $< $@

driver/compdynlink_common.mli: $(DYNLINK_DIR)/dynlink_common.mli \
    driver/compify_dynlink.sh
	driver/compify_dynlink.sh $< $@

driver/compdynlink_types.mli: $(DYNLINK_DIR)/dynlink_types.mli
	cp $(DYNLINK_DIR)/dynlink_types.mli driver/compdynlink_types.mli

driver/compdynlink_types.ml: $(DYNLINK_DIR)/dynlink_types.ml
	cp $(DYNLINK_DIR)/dynlink_types.ml driver/compdynlink_types.ml

driver/compdynlink_platform_intf.ml: $(DYNLINK_DIR)/dynlink_platform_intf.ml \
    driver/compify_dynlink.sh
	driver/compify_dynlink.sh $< $@

ifeq ($(NATDYNLINK),true)
driver/compdynlink.mlopt: $(DYNLINK_DIR)/natdynlink.ml driver/compdynlink.mli
	cat $(DYNLINK_DIR)/natdynlink.ml | \
	  sed 's/Dynlink_/Compdynlink_/g' \
	  > driver/compdynlink.mlopt
else
driver/compdynlink.mlopt: $(DYNLINK_DIR)/nodynlink.ml driver/compdynlink.mli
	cat $(DYNLINK_DIR)/nodynlink.ml | \
	  sed 's/Dynlink_/Compdynlink_/g' \
	  > driver/compdynlink.mlopt
endif

driver/compdynlink.mli: $(DYNLINK_DIR)/dynlink.mli \
    driver/compify_dynlink.sh
	driver/compify_dynlink.sh $< $@

# See comment in otherlibs/dynlink/Makefile about these two rules.
driver/compdynlink_platform_intf.mli: driver/compdynlink_platform_intf.ml
	cp $< $@

beforedepend:: driver/compdynlink.mlbyte \
               driver/compdynlink.mlopt \
               driver/compdynlink_platform_intf.ml \
               driver/compdynlink_platform_intf.mli \
               driver/compdynlink_types.ml \
               driver/compdynlink_types.mli \
               driver/compdynlink.mli \
               driver/compdynlink_common.ml \
               driver/compdynlink_common.mli
partialclean::
	rm -f driver/compdynlink.mlbyte
	rm -f driver/compdynlink.mlopt
	rm -f driver/compdynlink.mli
	rm -f driver/compdynlink_platform_intf.ml
	rm -f driver/compdynlink_platform_intf.mli
	rm -f driver/compdynlink_common.ml
	rm -f driver/compdynlink_common.mli
	rm -f driver/compdynlink_types.mli
	rm -f driver/compdynlink_types.ml

# The native toplevel

$(COMPLIBDIR)/ocaml_opttoplevel.ml: tools/gen_prefix CompilerModules
	$(CAMLRUN) $< -prefix ocaml_opttoplevel $(OPTTOPLEVEL) > $@
$(COMPLIBDIR)/ocaml_opttoplevel.cmo: $(COMPLIBDIR)/ocaml_opttoplevel.ml
	$(CAMLC) $(COMPFLAGS) -w -49 -c $<
$(COMPLIBDIR)/ocaml_opttoplevel.cmx: $(COMPLIBDIR)/ocaml_opttoplevel.ml
	$(CAMLOPT) $(COMPFLAGS) -w -49 -c $<
$(COMPLIBDIR)/ocaml_opttoplevel__genprintval.ml:
	echo 'include Ocaml_toplevel__genprintval' > $@
$(COMPLIBDIR)/ocamlopttoplevel.cmxa: $(OPTTOPLEVEL_CMO:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $^

# Legacy unprefixed ocamlopttoplevel

$(addprefix $(COMPLIBDIR_U)/,$(OPTTOPLEVEL_ML:=.ml) \
	$(OPTTOPLEVEL_MLI_ONLY:=.mli)): $(COMPLIBDIR_U)/opttoplevel
$(COMPLIBDIR_U)/ocamlopttoplevel.cmxa: \
	$(OPTTOPLEVEL_CMI_U) $(OPTTOPLEVEL_CMO_U:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(filter %.cmx,$^)
$(COMPLIBDIR_U)/opttoplevel: tools/gen_prefix CompilerModules
	$(CAMLRUN) $< $(filter-out %genprintval,$(OPTTOPLEVEL_ML)) \
	  -mli $(OPTTOPLEVEL_MLI_ONLY) \
	  -prefix ocaml_opttoplevel -unprefix $(COMPLIBDIR_U)
	touch $@

$(call copy_files_with_prefix,opttoplevel,$(filter-out toplevel/genprintval,\
$(OPTTOPLEVEL)),Ocaml_common Ocaml_bytecomp Ocaml_optcomp Ocaml_opttoplevel)

partialclean::
	rm -f $(COMPLIBDIR)/ocaml_opttoplevel.ml \
	  $(COMPLIBDIR)/ocamlopttoplevel.cmxa
	rm -f $(COMPLIBDIR_U)/ocamlopttoplevel.cmxa $(COMPLIBDIR_U)/opttoplevel

beforedepend:: $(COMPLIBDIR)/ocaml_opttoplevel.ml $(COMPLIBDIR_U)/opttoplevel

# When the native toplevel executable has an extension (e.g. ".exe"),
# provide a phony 'ocamlnat' synonym

ifneq ($(EXE),)
.PHONY: ocamlnat
ocamlnat: ocamlnat$(EXE)
endif

ocamlnat$(EXE): $(COMPLIBDIR_U)/ocamlcommon.cmxa \
    $(COMPLIBDIR_U)/ocamloptcomp.cmxa \
    $(COMPLIBDIR_U)/ocamlbytecomp.cmxa \
    $(COMPLIBDIR_U)/ocamlopttoplevel.cmxa \
    $(OPTTOPLEVELSTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -linkall -o $@ $^

partialclean::
	rm -f ocamlnat$(EXE)

$(COMPLIBDIR)/ocaml_opttoplevel__opttoploop.cmx: otherlibs/dynlink/dynlink.cmxa

# The numeric opcodes

bytecomp/opcodes.ml: runtime/caml/instruct.h tools/make_opcodes
	runtime/ocamlrun tools/make_opcodes -opcodes < $< > $@

tools/make_opcodes: tools/make_opcodes.mll
	$(MAKE) -C tools make_opcodes

partialclean::
	rm -f bytecomp/opcodes.ml

beforedepend:: bytecomp/opcodes.ml

# Testing the parser -- see parsing/HACKING.adoc

SOURCE_FILES=$(shell git ls-files '*.ml' '*.mli' | grep -v boot/menhir/parser)

AST_FILES=$(addsuffix .ast,$(SOURCE_FILES))

build-all-asts: $(AST_FILES)

CAMLC_DPARSETREE := \
	$(CAMLRUN) ./ocamlc -nostdlib -nopervasives \
	  -stop-after parsing -dparsetree

%.ml.ast: %.ml ocamlc
	$(CAMLC_DPARSETREE) $< 2> $@ || exit 0
# `|| exit 0` : some source files will fail to parse
# (for example, they are meant as toplevel scripts
# rather than source files, or are parse-error tests),
# we ignore the failure in that case

%.mli.ast: %.mli ocamlc
	$(CAMLC_DPARSETREE) $< 2> $@ || exit 0

.PHONY: list-all-asts
list-all-asts:
	@for f in $(AST_FILES); do echo "'$$f'"; done

partialclean::
	rm -f $(AST_FILES)

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) $(INCLUDES) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) $(INCLUDES) -c $<

.ml.cmx: ocamlopt
	$(CAMLOPT) $(COMPFLAGS) $(INCLUDES) -c $<

partialclean::
	for d in utils parsing typing bytecomp asmcomp middle_end \
	         middle_end/base_types asmcomp/debug driver toplevel tools \
	         $(COMPLIBDIR) $(COMPLIBDIR_U); do \
	  rm -f $$d/*.cm[ioxt] $$d/*.cmti $$d/*.annot $$d/*.$(S) \
	    $$d/*.$(O) $$d/*.$(SO) $d/*~; \
	done
	rm -f *~

MAPS=\
  -map $(COMPLIBDIR)/ocaml_common.ml \
  -map $(COMPLIBDIR)/ocaml_bytecomp.ml \
  -map $(COMPLIBDIR)/ocaml_toplevel.ml \
  -map $(COMPLIBDIR)/ocaml_optcomp.ml \
  -map $(COMPLIBDIR)/ocaml_opttoplevel.ml

.PHONY: depend
depend: beforedepend
	(for d in driver toplevel; do \
	 $(CAMLDEP) $(DEPFLAGS) -I $(COMPLIBDIR_U) $$d/*.mli $$d/*.ml || exit; \
	 done) > .depend
	(for d in common bytecomp toplevel optcomp opttoplevel; \
	 do $(CAMLDEP) $(DEPFLAGS) $(MAPS) -I $(COMPLIBDIR) \
	  $(COMPLIBDIR)/ocaml_$${d}__*.mli \
	  $(COMPLIBDIR)/ocaml_$${d}__*.ml || exit; \
	 done) >> .depend
	$(CAMLDEP) $(DEPFLAGS) $(MAPS) -I $(COMPLIBDIR) -native \
	 -impl $(COMPLIBDIR)/ocaml_common__compdynlink.mlopt >> .depend
	$(CAMLDEP) $(DEPFLAGS) $(MAPS) -I $(COMPLIBDIR) -bytecode \
	 -impl $(COMPLIBDIR)/ocaml_common__compdynlink.mlbyte >> .depend
	$(CAMLDEP) $(DEPFLAGS) $(MAPS) -I $(COMPLIBDIR) \
	 $(COMPLIBDIR_U)/*.mli $(COMPLIBDIR_U)/*.ml >> .depend

.PHONY: distclean
distclean: clean
	rm -f boot/ocamlrun boot/ocamlrun$(EXE) boot/camlheader \
	boot/*.cm* boot/libcamlrun.$(A)
	rm -f Makefile.config runtime/caml/m.h runtime/caml/s.h
	rm -f tools/*.bak
	rm -f ocaml ocamlc
	rm -f testsuite/_log*

include .depend
