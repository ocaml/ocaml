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

COMPFLAGS=-strict-sequence -principal -absname -w +a-4-9-41-42-44-45-48-66 \
	  -warn-error A \
          -bin-annot -safe-string -strict-formats
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

UTILS=utils/config utils/build_path_prefix_map utils/misc \
  utils/identifiable utils/numbers utils/arg_helper \
  utils/clflags utils/profile \
  utils/terminfo utils/ccomp utils/warnings \
  utils/consistbl \
  utils/strongly_connected_components \
  utils/targetint

PARSING=parsing/asttypes parsing/parsetree \
  parsing/location parsing/longident \
  parsing/docstrings parsing/syntaxerr \
  parsing/ast_helper \
  parsing/pprintast \
  parsing/camlinternalMenhirLib parsing/parser \
  parsing/lexer parsing/parse parsing/printast \
  parsing/ast_mapper parsing/ast_iterator parsing/attr_helper \
  parsing/builtin_attributes parsing/ast_invariants parsing/depend

MLI_ONLY=\
  parsing/asttypes \
  parsing/parsetree \
  typing/outcometree \
  typing/annot \
  bytecomp/cmo_format \
  asmcomp/cmx_format \
  asmcomp/x86_ast \
  middle_end/backend_intf \
  middle_end/simplify_boxed_integer_ops_intf \
  middle_end/inlining_decision_intf

ML_ONLY=\
  bytecomp/opcodes \
  asmcomp/arch \
  asmcomp/CSE \
  asmcomp/branch_relaxation_intf

TYPING=typing/outcometree typing/annot \
  typing/ident typing/path \
  typing/primitive typing/types \
  typing/btype typing/oprint \
  typing/subst typing/predef \
  typing/datarepr typing/cmi_format typing/env \
  typing/typedtree typing/printtyped typing/ctype \
  typing/printtyp typing/includeclass \
  typing/mtype typing/envaux typing/includecore \
  typing/typedtreeIter typing/tast_mapper \
  typing/cmt_format typing/untypeast \
  typing/includemod typing/typetexp typing/printpat \
  typing/parmatch typing/stypes \
  typing/typedecl_properties typing/typedecl_variance \
  typing/typedecl_unboxed typing/typedecl_immediacy \
  typing/typedecl typing/typeopt \
  typing/rec_check typing/typecore typing/typeclass \
  typing/typemod

COMP=bytecomp/cmo_format bytecomp/lambda bytecomp/printlambda \
  bytecomp/semantics_of_primitives \
  bytecomp/switch bytecomp/matching \
  bytecomp/translobj bytecomp/translattribute \
  bytecomp/translprim bytecomp/translcore \
  bytecomp/translclass bytecomp/translmod \
  bytecomp/simplif bytecomp/runtimedef \
  bytecomp/meta bytecomp/opcodes \
  bytecomp/bytesections bytecomp/dll \
  bytecomp/symtable \
  driver/pparse driver/main_args \
  driver/compenv driver/compmisc \
  driver/compdynlink_types driver/compdynlink_platform_intf \
  driver/compdynlink_common driver/compdynlink \
  driver/compplugin driver/makedepend \
  driver/compile_common


COMMON=$(UTILS) $(PARSING) $(TYPING) $(COMP)

BYTECOMP=bytecomp/instruct bytecomp/bytegen \
  bytecomp/printinstr bytecomp/emitcode \
  bytecomp/bytelink bytecomp/bytelibrarian bytecomp/bytepackager \
  driver/errors driver/compile

ARCH_SPECIFIC =\
  asmcomp/arch.ml asmcomp/proc.ml asmcomp/CSE.ml asmcomp/selection.ml \
  asmcomp/scheduling.ml asmcomp/reload.ml

INTEL_ASM=\
  asmcomp/x86_ast \
  asmcomp/x86_proc \
  asmcomp/x86_dsl \
  asmcomp/x86_gas \
  asmcomp/x86_masm

ARCH_SPECIFIC_ASMCOMP=
ifeq ($(ARCH),i386)
ARCH_SPECIFIC_ASMCOMP=$(INTEL_ASM)
endif
ifeq ($(ARCH),amd64)
ARCH_SPECIFIC_ASMCOMP=$(INTEL_ASM)
endif

ASMCOMP=\
  $(ARCH_SPECIFIC_ASMCOMP) \
  asmcomp/cmx_format \
  asmcomp/arch \
  asmcomp/backend_var \
  asmcomp/cmm asmcomp/printcmm \
  asmcomp/reg asmcomp/debug/reg_with_debug_info \
  asmcomp/debug/reg_availability_set \
  asmcomp/mach asmcomp/proc \
  asmcomp/clambda asmcomp/printclambda \
  asmcomp/export_info \
  asmcomp/export_info_for_pack \
  asmcomp/compilenv \
  asmcomp/closure \
  asmcomp/traverse_for_exported_symbols \
  asmcomp/build_export_info \
  asmcomp/closure_offsets \
  asmcomp/flambda_to_clambda \
  asmcomp/import_approx \
  asmcomp/un_anf \
  asmcomp/afl_instrument \
  asmcomp/strmatch asmcomp/cmmgen \
  asmcomp/interval \
  asmcomp/printmach asmcomp/selectgen \
  asmcomp/spacetime_profiling asmcomp/selection \
  asmcomp/comballoc \
  asmcomp/CSEgen asmcomp/CSE \
  asmcomp/liveness \
  asmcomp/spill asmcomp/split \
  asmcomp/interf asmcomp/coloring \
  asmcomp/linscan \
  asmcomp/reloadgen asmcomp/reload \
  asmcomp/deadcode \
  asmcomp/printlinear asmcomp/linearize \
  asmcomp/debug/available_regs \
  asmcomp/schedgen asmcomp/scheduling \
  asmcomp/branch_relaxation_intf \
  asmcomp/branch_relaxation \
  asmcomp/emitaux asmcomp/emit asmcomp/asmgen \
  asmcomp/asmlink asmcomp/asmlibrarian asmcomp/asmpackager \
  driver/opterrors driver/optcompile

MIDDLE_END=\
  middle_end/backend_intf \
  middle_end/simplify_boxed_integer_ops_intf \
  middle_end/inlining_decision_intf \
  middle_end/int_replace_polymorphic_compare \
  middle_end/debuginfo \
  middle_end/base_types/tag \
  middle_end/base_types/linkage_name \
  middle_end/base_types/compilation_unit \
  middle_end/internal_variable_names \
  middle_end/base_types/variable \
  middle_end/base_types/mutable_variable \
  middle_end/base_types/id_types \
  middle_end/base_types/set_of_closures_id \
  middle_end/base_types/set_of_closures_origin \
  middle_end/base_types/closure_element \
  middle_end/base_types/closure_id \
  middle_end/base_types/closure_origin \
  middle_end/base_types/var_within_closure \
  middle_end/base_types/static_exception \
  middle_end/base_types/export_id \
  middle_end/base_types/symbol \
  middle_end/pass_wrapper \
  middle_end/allocated_const \
  middle_end/parameter \
  middle_end/projection \
  middle_end/flambda \
  middle_end/flambda_iterators \
  middle_end/flambda_utils \
  middle_end/inlining_cost \
  middle_end/effect_analysis \
  middle_end/freshening \
  middle_end/simple_value_approx \
  middle_end/lift_code \
  middle_end/closure_conversion_aux \
  middle_end/closure_conversion \
  middle_end/initialize_symbol_to_let_symbol \
  middle_end/lift_let_to_initialize_symbol \
  middle_end/find_recursive_functions \
  middle_end/invariant_params \
  middle_end/inconstant_idents \
  middle_end/alias_analysis \
  middle_end/lift_constants \
  middle_end/share_constants \
  middle_end/simplify_common \
  middle_end/remove_unused_arguments \
  middle_end/remove_unused_closure_vars \
  middle_end/remove_unused_program_constructs \
  middle_end/simplify_boxed_integer_ops \
  middle_end/simplify_primitives \
  middle_end/inlining_stats_types \
  middle_end/inlining_stats \
  middle_end/inline_and_simplify_aux \
  middle_end/remove_free_vars_equal_to_args \
  middle_end/extract_projections \
  middle_end/augment_specialised_args \
  middle_end/unbox_free_vars_of_closures \
  middle_end/unbox_specialised_args \
  middle_end/unbox_closures \
  middle_end/inlining_transforms \
  middle_end/inlining_decision \
  middle_end/inline_and_simplify \
  middle_end/ref_to_variables \
  middle_end/flambda_invariants \
  middle_end/middle_end

OPTCOMP=$(MIDDLE_END) $(ASMCOMP)

TOPLEVEL=toplevel/genprintval toplevel/toploop \
  toplevel/trace toplevel/topdirs toplevel/topmain

OPTTOPLEVEL=toplevel/genprintval toplevel/opttoploop \
  toplevel/opttopdirs toplevel/opttopmain
BYTESTART=driver/main.cmo

OPTSTART=driver/optmain.cmo

TOPLEVELSTART=toplevel/topstart.cmo

OPTTOPLEVELSTART=toplevel/opttopstart.cmo

PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

COMPLIBDIR=compilerlibs
COMPLIBDIR_U=unprefixed_compilerlibs

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
	$(MAKE) prefixed_compilerlibs.opt
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
	$(MAKE) prefixed_compilerlibs
	$(MAKE) prefixed_compilerlibs.opt
	$(MAKE) prefixed_compilerlibs.optopt
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
	$(MAKE) prefixed_compilerlibs
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
	   utils/*.cmi \
	   parsing/*.cmi \
	   typing/*.cmi \
	   bytecomp/*.cmi \
	   driver/*.cmi \
	   toplevel/*.cmi \
	   "$(INSTALL_COMPLIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   utils/*.cmt utils/*.cmti utils/*.mli \
	   parsing/*.cmt parsing/*.cmti parsing/*.mli \
	   typing/*.cmt typing/*.cmti typing/*.mli \
	   bytecomp/*.cmt bytecomp/*.cmti bytecomp/*.mli \
	   driver/*.cmt driver/*.cmti driver/*.mli \
	   toplevel/*.cmt toplevel/*.cmti toplevel/*.mli \
	   "$(INSTALL_COMPLIBDIR)"
endif
	$(INSTALL_DATA) \
	   $(COMPLIBDIR_U)/ocamlcommon.cma $(COMPLIBDIR_U)/ocamlbytecomp.cma \
	   $(COMPLIBDIR_U)/ocamltoplevel.cma $(BYTESTART) $(TOPLEVELSTART) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_common__*.cmi "$(INSTALL_OCAMLCOMMONDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_bytecomp__*.cmi "$(INSTALL_OCAMLBYTECOMPDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_optcomp__*.cmi "$(INSTALL_OCAMLTOPLEVELDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_common__*.cmt \
	  $(COMPLIBDIR)/ocaml_common__*.cmti \
	  $(COMPLIBDIR)/ocaml_common__*.mli \
	  "$(INSTALL_OCAMLCOMMONDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_bytecomp__*.cmt \
	  $(COMPLIBDIR)/ocaml_bytecomp__*.cmti \
	  $(COMPLIBDIR)/ocaml_bytecomp__*.mli \
	  "$(INSTALL_OCAMLBYTECOMPDIR)"
	$(INSTALL_DATA) \
	  $(COMPLIBDIR)/ocaml_toplevel__*.cmt \
	  $(COMPLIBDIR)/ocaml_toplevel__*.cmti \
	  $(COMPLIBDIR)/ocaml_toplevel__*.mli \
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
	    middle_end/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/base_types/*.cmi \
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
	    middle_end/base_types/*.cmt middle_end/base_types/*.cmti \
	    middle_end/base_types/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    asmcomp/*.cmt asmcomp/*.cmti \
	    asmcomp/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
endif
	$(INSTALL_DATA) \
	    $(COMPLIBDIR_U)/ocamloptcomp.cma $(OPTSTART) \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    $(COMPLIBDIR)/ocaml_optcomp__*.cmi \
	    "$(INSTALL_OCAMLOPTCOMPDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	    $(COMPLIBDIR)/ocaml_optcomp__*.cmt \
	    $(COMPLIBDIR)/ocaml_optcomp__*.cmti \
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
	   utils/*.cmx parsing/*.cmx typing/*.cmx bytecomp/*.cmx \
	   driver/*.cmx asmcomp/*.cmx middle_end/*.cmx \
	   middle_end/base_types/*.cmx "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
           $(COMPLIBDIR_U)/ocamlcommon.cmxa $(COMPLIBDIR_U)/ocamlcommon.$(A) \
	   $(COMPLIBDIR_U)/ocamlbytecomp.cmxa \
	   $(COMPLIBDIR_U)/ocamlbytecomp.$(A) \
	   $(COMPLIBDIR_U)/ocamloptcomp.cmxa $(COMPLIBDIR_U)/ocamloptcomp.$(A) \
	   $(BYTESTART:.cmo=.cmx) $(BYTESTART:.cmo=.$(O)) \
	   $(OPTSTART:.cmo=.cmx) $(OPTSTART:.cmo=.$(O)) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/ocaml_common__*.cmx \
	   "$(INSTALL_OCAMLCOMMONDIR)"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/ocaml_bytecomp__*.cmx \
	   "$(INSTALL_OCAMLBYTECOMPDIR)"
	$(INSTALL_DATA) \
	   $(COMPLIBDIR)/ocaml_optcomp__*.cmx \
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
	     toplevel/opttopdirs.cmi \
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

OPENS=-open Ocaml_common \
      -open Ocaml_bytecomp \
      -open Ocaml_optcomp \
      -open Ocaml_toplevel
P_COMPFLAGS=$(COMPFLAGS) -I $(COMPLIBDIR) \
    -no-alias-deps $(OPENS)
P_DEPFLAGS=\
    $(patsubst %,-map $(COMPLIBDIR)/ocaml_%.ml, \
      common bytecomp optcomp toplevel) $(OPENS)
P_DEPINCLUDES=-I $(COMPLIBDIR)

$(COMPLIBDIR)/%.cmi: $(COMPLIBDIR)/%.mli
	$(CAMLC) $(P_COMPFLAGS) -c $<

$(COMPLIBDIR)/%.cmo: $(COMPLIBDIR)/%.ml
	$(CAMLC) $(P_COMPFLAGS) -c $<

$(COMPLIBDIR)/%.cmx: $(COMPLIBDIR)/%.ml
	$(CAMLOPT) $(P_COMPFLAGS) -c $<

define cp
$(COMPLIBDIR)/ocaml_$(1)__$(notdir $(2)): $(2)
	cp $$< $$@

beforedepend:: $(COMPLIBDIR)/ocaml_$(1)__$(notdir $(2))
endef

tools/gen_prefix: tools/gen_prefix.ml
	$(CAMLC) -o $@ $<

define f
$(foreach f,\
  $(filter-out $(MLI_ONLY:=.ml),\
     $(patsubst driver/compdynlink.ml,\
       driver/compdynlink.mlbyte driver/compdynlink.mlopt,$($(2):=.ml))) \
  $(filter-out $(ML_ONLY:=.mli),$($(2):=.mli)),\
  $(eval $(call cp,$(1),$(f))))

$(2)_CMO:=$(notdir $(filter-out $(MLI_ONLY:=.cmo),$($(2):=.cmo)))
$(2)_CMX:=$(notdir $(filter-out $(MLI_ONLY:=.cmx),$($(2):=.cmx)))
$(2)_CMI_ONLY:=$(notdir $(filter $(MLI_ONLY:=.cmi),$($(2):=.cmi)))

$$(COMPLIBDIR)/ocaml_$(1).ml: tools/gen_prefix
	$$(CAMLRUN) $$< -prefix $(1) $($(2)) > $$@

beforedepend:: $$(COMPLIBDIR)/ocaml_$(1).ml

$$(COMPLIBDIR)/ocaml_$(1).cmo: $$(COMPLIBDIR)/ocaml_$(1).ml
	$$(CAMLC) $$(COMPFLAGS) -no-alias-deps -w -49 -c $$<

$$(COMPLIBDIR)/ocaml_$(1).cmx: $$(COMPLIBDIR)/ocaml_$(1).ml
	$$(CAMLOPT) $$(COMPFLAGS) -no-alias-deps -w -49 -c $$<

$$(COMPLIBDIR)/ocaml$(1).cma: $$(COMPLIBDIR)/ocaml_$(1).cmo \
    $(addprefix $$(COMPLIBDIR)/ocaml_$(1)__,$($(2)_CMO))
	$$(CAMLC) -a -linkall -o $$@ $$^

$$(COMPLIBDIR)/ocaml$(1).cmxa: $$(COMPLIBDIR)/ocaml_$(1).cmx \
    $(addprefix $$(COMPLIBDIR)/ocaml_$(1)__,$($(2)_CMX))
	$$(CAMLOPT) -a -linkall -o $$@ $$^

$$(COMPLIBDIR_U)/ocaml$(1).cma: unprefixed_sources \
    $$(addprefix $$(COMPLIBDIR_U)/,$$($(2)_CMI_ONLY)) \
    $$(COMPLIBDIR)/ocaml_$(1).cmo \
    $$(addprefix $$(COMPLIBDIR)/ocaml_$(1)__,$$($(2)_CMO)) \
    $$(addprefix $$(COMPLIBDIR_U)/,$$($(2)_CMO))
	$$(CAMLC) -a -linkall -o $$@ $$(filter %.cmo, $$^)

$$(COMPLIBDIR_U)/ocaml$(1).cmxa: unprefixed_sources \
    $$(addprefix $$(COMPLIBDIR_U)/,$$($(2)_CMI_ONLY)) \
    $$(COMPLIBDIR)/ocaml_$(1).cmx \
    $$(addprefix $$(COMPLIBDIR)/ocaml_$(1)__,$$($(2)_CMX)) \
    $$(addprefix $$(COMPLIBDIR_U)/,$$($(2)_CMX))
	$$(CAMLOPT) -a -linkall -o $$@ $$(filter %.cmx, $$^)

partialclean::
	rm -f $$(COMPLIBDIR)/ocaml_$(1).{ml,cm*}
	rm -f $$(COMPLIBDIR_U)/ocaml$(1).{cm*,$$(A)}
endef

.PHONY: force_unprefixed_sources
force_unprefixed_sources: tools/gen_prefix
	$(CAMLRUN) $< $(filter-out $(MLI_ONLY), $(COMMON)) \
	  -mli $(filter $(MLI_ONLY), $(COMMON)) -prefix ocaml_common \
	  -unprefix $(COMPLIBDIR_U)
	$(CAMLRUN) $< $(filter-out $(MLI_ONLY), $(BYTECOMP)) \
	  -mli $(filter $(MLI_ONLY), $(BYTECOMP)) -prefix ocaml_bytecomp \
	  -unprefix $(COMPLIBDIR_U)
	$(CAMLRUN) $< $(filter-out $(MLI_ONLY), $(TOPLEVEL)) \
	  -mli $(filter $(MLI_ONLY), $(TOPLEVEL)) -prefix ocaml_toplevel \
	  -unprefix $(COMPLIBDIR_U)
	$(CAMLRUN) $< $(filter-out $(MLI_ONLY), $(OPTCOMP)) \
	  -mli $(filter $(MLI_ONLY), $(OPTCOMP)) -prefix ocaml_optcomp \
	  -unprefix $(COMPLIBDIR_U)

unprefixed_sources:
	$(MAKE) force_unprefixed_sources
	touch unprefixed_sources

beforedepend:: unprefixed_sources

partialclean::
	rm -f $(COMPLIBDIR_U)/*.ml*
	rm -f $(COMPLIBDIR_U)/*.cm*

$(COMPLIBDIR_U)/%.cmi: $(COMPLIBDIR_U)/%.mli
	$(CAMLC) $(COMPFLAGS) -I $(COMPLIBDIR) -c $<

$(COMPLIBDIR_U)/%.cmo: $(COMPLIBDIR_U)/%.ml
	$(CAMLC) $(COMPFLAGS) -I $(COMPLIBDIR) -c $<

$(COMPLIBDIR_U)/%.cmx: $(COMPLIBDIR_U)/%.ml
	$(CAMLOPT) $(COMPFLAGS) -I $(COMPLIBDIR) -c $<

partialclean::
	rm -f $(COMPLIBDIR)/ocaml_*__*.{ml*,cm*}

$(COMPLIBDIR)/ocaml_common__compdynlink.cmo: \
    $(COMPLIBDIR)/ocaml_common__compdynlink.mlbyte
	$(CAMLC) $(P_COMPFLAGS) -c -impl $<

$(COMPLIBDIR)/ocaml_common__compdynlink.cmx: \
    $(COMPLIBDIR)/ocaml_common__compdynlink.mlopt
	$(CAMLOPT) $(P_COMPFLAGS) -c -impl $<

$(eval $(call f,common,COMMON))
$(eval $(call f,bytecomp,BYTECOMP))
$(eval $(call f,optcomp,OPTCOMP))
$(eval $(call f,toplevel,TOPLEVEL))

.PHONY: prefixed_compilerlibs
prefixed_compilerlibs: \
    $(COMPLIBDIR)/ocamlcommon.cma \
    $(COMPLIBDIR)/ocamlbytecomp.cma \
    $(COMPLIBDIR)/ocamltoplevel.cma

.PHONY: prefixed_compilerlibs.opt
prefixed_compilerlibs.opt: \
    $(COMPLIBDIR)/ocamloptcomp.cma

.PHONY: prefixed_compilerlibs.optopt
prefixed_compilerlibs.optopt: \
    $(COMPLIBDIR)/ocamlcommon.cmxa \
    $(COMPLIBDIR)/ocamlbytecomp.cmxa \
    $(COMPLIBDIR)/ocamltoplevel.cmxa \
    $(COMPLIBDIR)/ocamloptcomp.cmxa

# The bytecode compiler

ocamlc: $(COMPLIBDIR_U)/ocamlcommon.cma $(COMPLIBDIR_U)/ocamlbytecomp.cma \
	$(BYTESTART)
	$(CAMLC) $(LINKFLAGS) -compat-32 -o $@ $^

partialclean::
	rm -rf ocamlc

# The native-code compiler

ocamlopt: $(COMPLIBDIR_U)/ocamlcommon.cma $(COMPLIBDIR_U)/ocamloptcomp.cma \
          $(OPTSTART)
	$(CAMLC) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt

# The toplevel

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

# The bytecode compiler compiled with the native-code compiler

ocamlc.opt: $(COMPLIBDIR_U)/ocamlcommon.cmxa \
	    $(COMPLIBDIR_U)/ocamlbytecomp.cmxa \
            $(BYTESTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -o $@ $^ -cclib "$(BYTECCLIBS)"

partialclean::
	rm -f ocamlc.opt

# The native-code compiler compiled with itself

ocamlopt.opt: $(COMPLIBDIR_U)/ocamlcommon.cmxa \
	      $(COMPLIBDIR_U)/ocamloptcomp.cmxa \
              $(OPTSTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt.opt

$(COMMON_CMX) $(BYTECOMP_CMX) $(OPTCOMP_CMX): ocamlopt

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

compilerlibs/ocamlmiddleend.cma: $(MIDDLE_END_CMO)
	$(CAMLC) -a -o $@ $^
compilerlibs/ocamlmiddleend.cmxa: $(MIDDLE_END_CMX)
	$(CAMLOPT) -a -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlmiddleend.cma \
	      compilerlibs/ocamlmiddleend.cmxa \
	      compilerlibs/ocamlmiddleend.$(A)

# Tools

.PHONY: ocamltools
ocamltools: ocamlc ocamllex asmcomp/cmx_format.cmi \
            asmcomp/printclambda.cmo compilerlibs/ocamlmiddleend.cma \
            asmcomp/export_info.cmo
	$(MAKE) -C tools all

.PHONY: ocamltoolsopt
ocamltoolsopt: ocamlopt
	$(MAKE) -C tools opt

.PHONY: ocamltoolsopt.opt
ocamltoolsopt.opt: ocamlc.opt ocamllex.opt asmcomp/cmx_format.cmi \
                   asmcomp/printclambda.cmx compilerlibs/ocamlmiddleend.cmxa \
                   asmcomp/export_info.cmx
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

driver/compdynlink.cmo: driver/compdynlink.mlbyte
	$(CAMLC) $(COMPFLAGS) $(INCLUDES) -c -impl $<

driver/compdynlink.cmx: driver/compdynlink.mlopt
	$(CAMLOPT) $(COMPFLAGS) $(INCLUDES) -c -impl $<

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

driver/%.cmi: driver/%.mli
	$(CAMLC) $(COMPFLAGS) \
	  -I $(COMPLIBDIR) -I $(COMPLIBDIR_U) -I driver -c $<

driver/%.cmo: driver/%.ml
	$(CAMLC) $(COMPFLAGS) \
	  -I $(COMPLIBDIR) -I $(COMPLIBDIR_U) -I driver -c $<

driver/%.cmx: driver/%.ml
	$(CAMLOPT) $(COMPFLAGS) \
	  -I $(COMPLIBDIR) -I $(COMPLIBDIR_U) -I driver -c $<

toplevel/%.cmi: toplevel/%.mli
	$(CAMLC) $(COMPFLAGS) \
	  -I $(COMPLIBDIR) -I $(COMPLIBDIR_U) -I toplevel -c $<

toplevel/%.cmo: toplevel/%.ml
	$(CAMLC) $(COMPFLAGS) \
	  -I $(COMPLIBDIR) -I $(COMPLIBDIR_U) -I toplevel -c $<

toplevel/%.cmx: toplevel/%.ml
	$(CAMLOPT) $(COMPFLAGS) \
	  -I $(COMPLIBDIR) -I $(COMPLIBDIR_U) -I toplevel -c $<

partialclean::
	for d in utils parsing typing bytecomp asmcomp middle_end \
	         middle_end/base_types asmcomp/debug driver toplevel tools; do \
	  rm -f $$d/*.cm[ioxt] $$d/*.cmti $$d/*.annot $$d/*.$(S) \
	    $$d/*.$(O) $$d/*.$(SO) $d/*~; \
	done
	rm -f *~

.PHONY: depend
depend: beforedepend
	(for d in driver toplevel; do \
	 $(CAMLDEP) $(DEPFLAGS) -I $(COMPLIBDIR_U) $$d/*.mli $$d/*.ml || exit; \
	 done) > .depend
	(for d in common bytecomp toplevel optcomp; \
	 do $(CAMLDEP) $(DEPFLAGS) $(P_DEPFLAGS) $(P_DEPINCLUDES) \
          $(COMPLIBDIR)/ocaml_$${d}__*.{ml,mli} || exit; \
	 done) >> .depend
	$(CAMLDEP) $(DEPFLAGS) $(P_DEPFLAGS) $(P_DEPINCLUDES) -native \
	 -impl $(COMPLIBDIR)/ocaml_common__compdynlink.mlopt >> .depend
	$(CAMLDEP) $(DEPFLAGS) $(P_DEPFLAGS) $(P_DEPINCLUDES) -bytecode \
	 -impl $(COMPLIBDIR)/ocaml_common__compdynlink.mlbyte >> .depend
	$(CAMLDEP) $(DEPFLAGS) -I $(COMPLIBDIR) $(P_DEPFLAGS) \
	 $(COMPLIBDIR_U)/*.{ml,mli} >> .depend

.PHONY: distclean
distclean: clean
	rm -f boot/ocamlrun boot/ocamlrun$(EXE) boot/camlheader \
	boot/*.cm* boot/libcamlrun.$(A)
	rm -f Makefile.config runtime/caml/m.h runtime/caml/s.h
	rm -f tools/*.bak
	rm -f ocaml ocamlc
	rm -f testsuite/_log*

include .depend
