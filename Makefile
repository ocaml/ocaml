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
CAMLOPT=$(CAMLRUN) ./ocamlopt$(EXE) -g -nostdlib -I stdlib -I otherlibs/dynlink
ARCHES=amd64 i386 arm arm64 power s390x riscv
INCLUDES=-I utils -I parsing -I typing -I bytecomp -I file_formats \
        -I lambda -I middle_end -I middle_end/closure \
        -I middle_end/flambda -I middle_end/flambda/base_types \
        -I asmcomp -I asmcomp/debug \
        -I driver -I toplevel

COMPFLAGS=-strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48-66 \
	  -warn-error A \
          -bin-annot -safe-string -strict-formats $(INCLUDES)
LINKFLAGS=

ifeq "$(strip $(NATDYNLINKOPTS))" ""
OCAML_NATDYNLINKOPTS=
else
OCAML_NATDYNLINKOPTS = -ccopt "$(NATDYNLINKOPTS)"
endif

CAMLLEX=$(CAMLRUN) boot/ocamllex
CAMLDEP=$(CAMLRUN) boot/ocamlc -depend
DEPFLAGS=-slash
DEPINCLUDES=$(INCLUDES)

OCAMLDOC_OPT=$(WITH_OCAMLDOC:=.opt)
OCAMLTEST_OPT=$(WITH_OCAMLTEST:=.opt)

BYTESTART=driver/main.cmo

OPTSTART=driver/optmain.cmo

TOPLEVELSTART=toplevel/topstart.cmo

OPTTOPLEVELSTART=toplevel/opttopstart.cmo

PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

COMPLIBDIR=$(LIBDIR)/compiler-libs

TOPINCLUDES=$(addprefix -I otherlibs/,$(filter-out %threads,$(OTHERLIBRARIES)))
RUNTOP=./runtime/ocamlrun$(EXE) ./ocaml$(EXE) \
  -nostdlib -I stdlib -I toplevel \
  -noinit $(TOPFLAGS) $(TOPINCLUDES)
NATRUNTOP=./ocamlnat$(EXE) \
  -nostdlib -I stdlib -I toplevel \
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
else
  BOOT_FLEXLINK_CMD = \
    FLEXLINK_CMD="../boot/ocamlrun$(EXE) ../flexdll/flexlink.exe"
endif
else
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

configure: configure.ac aclocal.m4 VERSION tools/autogen
	tools/autogen

.PHONY: partialclean
partialclean::
	rm -f utils/config.ml utils/domainstate.ml utils/domainstate.mli

.PHONY: beforedepend
beforedepend:: utils/config.ml utils/domainstate.ml utils/domainstate.mli

programs := expunge ocaml ocamlc ocamlc.opt ocamlnat ocamlopt ocamlopt.opt

$(foreach program, $(programs), $(eval $(call PROGRAM_SYNONYM,$(program))))

# Start up the system from the distribution compiler
.PHONY: coldstart
coldstart:
	$(MAKE) -C runtime $(BOOT_FLEXLINK_CMD) all
	cp runtime/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) \
	  CAMLC='$$(BOOT_OCAMLC) -use-prims ../runtime/primitives' all
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

CMPBYT := $(CAMLRUN) tools/cmpbyt$(EXE)

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
promote: PROMOTE = $(CAMLRUN) tools/stripdebug
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
	$(MAKE) runtime
	$(MAKE) core
	$(MAKE) ocaml
	$(MAKE) opt-core
	$(MAKE) ocamlc.opt
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) \
	  $(WITH_OCAMLTEST)
	$(MAKE) ocamlopt.opt
	$(MAKE) otherlibrariesopt
	$(MAKE) ocamllex.opt ocamltoolsopt ocamltoolsopt.opt $(OCAMLDOC_OPT) \
	  $(OCAMLTEST_OPT)
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
	$(MAKE) CAMLRUN=runtime/ocamlrun$(EXE) promote
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
FLEXLINK_OCAMLOPT = \
   ../boot/ocamlrun$(EXE) ../boot/ocamlc \
   -use-prims ../runtime/primitives -nostdlib -I ../boot

.PHONY: flexlink
flexlink: flexdll/Makefile
	$(MAKE) -C runtime BOOTSTRAPPING_FLEXLINK=yes ocamlrun$(EXE)
	cp runtime/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	$(MAKE) -C stdlib \
                COMPILER="../boot/ocamlc -use-prims ../runtime/primitives" \
                $(filter-out *.cmi,$(LIBFILES))
	cd stdlib && cp $(LIBFILES) ../boot/
	$(MAKE) -C flexdll MSVC_DETECT=0 OCAML_CONFIG_FILE=../Makefile.config \
	  CHAINS=$(FLEXDLL_CHAIN) NATDYNLINK=false \
	  OCAMLOPT="$(FLEXLINK_OCAMLOPT)" \
	  flexlink.exe
	$(MAKE) -C runtime clean
	$(MAKE) partialclean

.PHONY: flexlink.opt
flexlink.opt:
	cd flexdll && \
	mv flexlink.exe flexlink && \
	($(MAKE) OCAML_FLEXLINK="../boot/ocamlrun$(EXE) ./flexlink" \
	           MSVC_DETECT=0 OCAML_CONFIG_FILE=../Makefile.config \
	           OCAMLOPT="../ocamlopt.opt$(EXE) -nostdlib -I ../stdlib" \
	           flexlink.exe || \
	 (mv flexlink flexlink.exe && false)) && \
	mv flexlink.exe flexlink.opt && \
	mv flexlink flexlink.exe

INSTALL_COMPLIBDIR=$(DESTDIR)$(COMPLIBDIR)
INSTALL_FLEXDLLDIR=$(INSTALL_LIBDIR)/flexdll

.PHONY: install-flexdll
install-flexdll:
	$(INSTALL_PROG) flexdll/flexlink.exe "$(INSTALL_BINDIR)/flexlink$(EXE)"
ifneq "$(filter-out mingw,$(TOOLCHAIN))" ""
	$(INSTALL_DATA) flexdll/default$(filter-out _i386,_$(ARCH)).manifest \
    "$(INSTALL_BINDIR)/"
endif
	if test -n "$(wildcard flexdll/flexdll_*.$(O))" ; then \
	  $(MKDIR) "$(INSTALL_FLEXDLLDIR)" ; \
	  $(INSTALL_DATA) flexdll/flexdll_*.$(O) "$(INSTALL_FLEXDLLDIR)" ; \
	fi

# Installation
.PHONY: install
install:
	$(MKDIR) "$(INSTALL_BINDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR)"
	$(MKDIR) "$(INSTALL_STUBLIBDIR)"
	$(MKDIR) "$(INSTALL_COMPLIBDIR)"
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
	   toplevel/topdirs.cmt toplevel/topdirs.cmti \
           toplevel/topdirs.mli \
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
ifneq "$(WITH_OCAMLDOC)" ""
	$(MAKE) -C ocamldoc install
endif
	if test -n "$(WITH_DEBUGGER)"; then \
	  $(MAKE) -C debugger install; \
	fi
ifeq "$(UNIX_OR_WIN32)" "win32"
	if test -n "$(FLEXDLL_SUBMODULE_PRESENT)"; then \
	  $(MAKE) install-flexdll; \
	fi
endif
	$(INSTALL_DATA) Makefile.config "$(INSTALL_LIBDIR)"
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	if test -f ocamlopt$(EXE); then $(MAKE) installopt; else \
	   cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.byte$(EXE) ocamlc$(EXE); \
	   $(LN) ocamllex.byte$(EXE) ocamllex$(EXE); \
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
	$(INSTALL_DATA) \
	    asmcomp/debug/*.cmi \
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
	$(INSTALL_DATA) \
	    asmcomp/debug/*.cmt asmcomp/debug/*.cmti \
	    asmcomp/debug/*.mli \
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
	fi
else
	if test -f ocamlopt.opt$(EXE); then $(MAKE) installoptopt; fi
endif
	$(MAKE) -C tools installopt
	if test -f ocamlopt.opt$(EXE) -a -f flexdll/flexlink.opt ; then \
	  $(INSTALL_PROG) \
	    flexdll/flexlink.opt "$(INSTALL_BINDIR)/flexlink$(EXE)" ; \
	fi

.PHONY: installoptopt
installoptopt:
	$(INSTALL_PROG) ocamlc.opt$(EXE) "$(INSTALL_BINDIR)"
	$(INSTALL_PROG) ocamlopt.opt$(EXE) "$(INSTALL_BINDIR)"
	$(INSTALL_PROG) lex/ocamllex.opt$(EXE) "$(INSTALL_BINDIR)"
	cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.opt$(EXE) ocamlc$(EXE); \
	   $(LN) ocamlopt.opt$(EXE) ocamlopt$(EXE); \
	   $(LN) ocamllex.opt$(EXE) ocamllex$(EXE)
	$(INSTALL_DATA) \
	   utils/*.cmx parsing/*.cmx typing/*.cmx bytecomp/*.cmx \
	   file_formats/*.cmx \
	   lambda/*.cmx \
	   driver/*.cmx asmcomp/*.cmx middle_end/*.cmx \
           middle_end/closure/*.cmx \
           middle_end/flambda/*.cmx \
           middle_end/flambda/base_types/*.cmx \
	   asmcomp/debug/*.cmx \
          "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   compilerlibs/*.cmxa compilerlibs/*.$(A) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   $(BYTESTART:.cmo=.cmx) $(BYTESTART:.cmo=.$(O)) \
	   $(OPTSTART:.cmo=.cmx) $(OPTSTART:.cmo=.$(O)) \
	   "$(INSTALL_COMPLIBDIR)"
	if test -f ocamlnat$(EXE) ; then \
	  $(INSTALL_PROG) ocamlnat$(EXE) "$(INSTALL_BINDIR)"; \
	  $(INSTALL_DATA) \
	     toplevel/opttopdirs.cmi \
	     "$(INSTALL_LIBDIR)"; \
	  $(INSTALL_DATA) \
	     $(OPTTOPLEVELSTART:.cmo=.cmx) $(OPTTOPLEVELSTART:.cmo=.$(O)) \
	     "$(INSTALL_COMPLIBDIR)"; \
	fi
	cd "$(INSTALL_COMPLIBDIR)" && \
	   $(RANLIB) ocamlcommon.$(A) ocamlbytecomp.$(A) ocamloptcomp.$(A)

# Installation of the *.ml sources of compiler-libs
.PHONY: install-compiler-sources
install-compiler-sources:
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   utils/*.ml parsing/*.ml typing/*.ml bytecomp/*.ml driver/*.ml \
           file_formats/*.ml \
           lambda/*.ml \
	   toplevel/*.ml middle_end/*.ml middle_end/closure/*.ml \
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
	$(CAMLC) $(LINKFLAGS) -linkall -o $@ $^

ocaml$(EXE): $(expunge) ocaml.tmp
	- $(CAMLRUN) $^ $@ $(PERVASIVES)

partialclean::
	rm -f ocaml$(EXE)

.PHONY: runtop
runtop:
	$(MAKE) coldstart
	$(MAKE) ocamlc
	$(MAKE) otherlibraries
	$(MAKE) ocaml
	@$(EXTRAPATH) $(RLWRAP) $(RUNTOP)

.PHONY: natruntop
natruntop:
	$(MAKE) core
	$(MAKE) opt
	$(MAKE) ocamlnat
	@$(FLEXLINK_ENV) $(EXTRAPATH) $(RLWRAP) $(NATRUNTOP)

# Native dynlink

otherlibs/dynlink/dynlink.cmxa: otherlibs/dynlink/native/dynlink.ml
	$(MAKE) -C otherlibs/dynlink allopt

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) $(OCAMLLEX_FLAGS) $<

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
	$(CAMLRUN) $(cvt_emit) < $< >> $@ \
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
	  $(BOOT_FLEXLINK_CMD) CAMLRUN=../runtime/ocamlrun$(EXE) all

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

parsing/parser.ml: boot/menhir/parser.ml parsing/parser.mly \
  tools/check-parser-uptodate-or-warn.sh
	@-tools/check-parser-uptodate-or-warn.sh
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
	$(MAKE) -C ocamldoc $@
	@echo "documentation is in ./ocamldoc/stdlib_html/"

.PHONY: manpages
manpages:
	$(MAKE) -C ocamldoc $@

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
checkstack := tools/checkstack
checkstack: $(checkstack)$(EXE)
	$<

.INTERMEDIATE: $(checkstack)$(EXE) $(checkstack).$(O)
$(checkstack)$(EXE): $(checkstack).$(O)
	$(MKEXE) $(OUTPUTEXE)$@ $<
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

ocamlnat$(EXE): compilerlibs/ocamlcommon.cmxa compilerlibs/ocamloptcomp.cmxa \
    compilerlibs/ocamlbytecomp.cmxa \
    otherlibs/dynlink/dynlink.cmxa \
    compilerlibs/ocamlopttoplevel.cmxa \
    $(OPTTOPLEVELSTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -linkall -o $@ $^

partialclean::
	rm -f ocamlnat ocamlnat.exe

toplevel/opttoploop.cmx: otherlibs/dynlink/dynlink.cmxa

# The numeric opcodes

make_opcodes := tools/make_opcodes$(EXE)

bytecomp/opcodes.ml: runtime/caml/instruct.h $(make_opcodes)
	runtime/ocamlrun$(EXE) $(make_opcodes) -opcodes < $< > $@

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

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) $(OPTCOMPFLAGS) -c $<

partialclean::
	for d in utils parsing typing bytecomp asmcomp middle_end file_formats \
           lambda middle_end/closure middle_end/flambda \
           middle_end/flambda/base_types asmcomp/debug \
           driver toplevel tools; do \
	  rm -f $$d/*.cm[ioxt] $$d/*.cmti $$d/*.annot $$d/*.s $$d/*.asm \
	    $$d/*.o $$d/*.obj $$d/*.so $$d/*.dll; \
	done

.PHONY: depend
depend: beforedepend
	(for d in utils parsing typing bytecomp asmcomp middle_end \
         lambda file_formats middle_end/closure middle_end/flambda \
         middle_end/flambda/base_types asmcomp/debug \
         driver toplevel; \
         do $(CAMLDEP) $(DEPFLAGS) $(DEPINCLUDES) $$d/*.mli $$d/*.ml || exit; \
         done) > .depend

.PHONY: distclean
distclean: clean
	rm -f boot/ocamlrun boot/ocamlrun.exe boot/camlheader \
	boot/*.cm* boot/libcamlrun.a boot/libcamlrun.lib boot/ocamlc.opt
	rm -f Makefile.config Makefile.build_config
	rm -f runtime/caml/m.h runtime/caml/s.h
	rm -rf autom4te.cache
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
	@echo "	./configure"
	@echo "	make"
	@echo "	make install"
	@echo "should work."
	@false
