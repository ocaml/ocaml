#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                          Samuel Hym, Tarides                           *
#*                                                                        *
#*   Copyright 2024 Tarides                                               *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Recipes to build a cross compiler (_not_ cross-compiling the compiler), aka
# generating code that will run on `target`, assuming that a non-cross OCaml
# compiler (so targetting our build machine) of the same version is available in
# $PATH

# As the cross compiler will be linked with the _build_ version of
# libcomprmarsh, we cannot rely on the detection of zstd done during `configure`
# (as it would have detected the _target_ version). So we recover the flags to
# link with zstd of the non-cross compiler.
# Note that the shell call is a variable that is used only once, so it doesn't
# have to be lazier.
HOST_ZSTD_LIBS=ZSTD_LIBS=$(shell ocamlopt -config-var compression_c_libraries)

# The build system adds various include directories which pertain to the current
# tree, including -I runtime, which is necessary for -custom executables.
# ocamltest is always a -custom executable, but some others (ocamldoc; the
# debugger, etc.) are only -custom in specific circumstances. It is therefore
# fiddly to change this in the main build system, so we perform a slightly
# different trick by ensuring that "+" is the first entry in VPATH. This will
# put the host compiler's standard library (and consequently its runtime
# objects) well above the .a files found with -I runtime. For now, this seems
# the least nefarious way of ensuring that the bytecode compiler has the C
# headers in runtime/caml available without breaking builds with an external
# ocamlopt.
VPATH := + $(VPATH)

CROSS_OVERRIDES=OCAMLRUN=ocamlrun NEW_OCAMLRUN=ocamlrun \
  BOOT_OCAMLLEX=ocamllex OCAMLYACC=ocamlyacc
CROSS_COMPILER_OVERRIDES=$(CROSS_OVERRIDES) CAMLC=ocamlc CAMLOPT=ocamlopt \
  BEST_OCAMLC=ocamlc BEST_OCAMLOPT=ocamlopt BEST_OCAMLLEX=ocamllex
CROSS_COMPILERLIBS_OVERRIDES=$(CROSS_OVERRIDES) CAMLC=ocamlc \
  CAMLOPT="$(ROOTDIR)/ocamlopt.opt$(EXE) $(STDLIBFLAGS)"

ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
# Declare flexlink to be an 'old' file, so that make doesn't try to rebuild it
# with the build rules in `Makefile`; its build is driven by the `cross-flexdll`
# recipe provided here instead
OLDS := -o $(BYTE_BINDIR)/flexlink$(EXE)
else
OLDS :=
endif

# The compiler libs that should be rebuilt for target (they are first built for
# host as part of the .opt compilers)
CROSSCOMPILERLIBS := $(addprefix compilerlibs/,$(addsuffix .cmxa,\
    ocamlcommon ocamlmiddleend ocamlbytecomp ocamloptcomp ocamltoplevel))

.PHONY: crossopt
ifeq "$(BOOTSTRAPPING_FLEXDLL)" "true"
crossopt: cross-flexdll
	$(MAKE) runtime-all $(OLDS)
else
# In that case, $(OLDS) is empty, we can depend directly on runtime-all
crossopt: runtime-all
endif
	$(MAKE) ocamlc $(TOOLS_BYTECODE_TARGETS) expunge$(EXE) \
	  $(CROSS_COMPILER_OVERRIDES) $(OLDS)
	$(MAKE) library $(CROSS_OVERRIDES) $(OLDS)
	$(MAKE) ocamlyacc $(CROSS_OVERRIDES) $(OLDS)
	$(MAKE) ocamllex $(CROSS_COMPILER_OVERRIDES) $(OLDS)
	$(MAKE) ocaml $(CROSS_COMPILER_OVERRIDES) $(OLDS)
	$(MAKE) dynlink-all $(CROSS_OVERRIDES) $(OLDS)
	$(MAKE) -C otherlibs all $(CROSS_OVERRIDES) $(OLDS)
	$(MAKE) runtimeopt $(OLDS)
	$(MAKE) ocamlc.opt ocamlopt.opt $(TOOLS_NATIVE_TARGETS) \
	  $(CROSS_COMPILER_OVERRIDES) "$(HOST_ZSTD_LIBS)" $(OLDS)
	$(MAKE) libraryopt $(CROSS_OVERRIDES) $(OLDS)
	$(MAKE) otherlibrariesopt ocamltoolsopt $(CROSS_OVERRIDES) $(OLDS)
	$(MAKE) tools-allopt.opt $(CROSS_COMPILER_OVERRIDES) $(OLDS)
	# We now build the compiler libs again, but for target this time
	rm -f $(ocamlcommon_NCOBJS) $(ocamlmiddleend_NCOBJS) \
	  $(ocamlbytecomp_NCOBJS) $(ocamloptcomp_NCOBJS) \
	  $(ocamltoplevel_NCOBJS) $(CROSSCOMPILERLIBS)
	$(MAKE) $(CROSSCOMPILERLIBS) $(CROSS_COMPILERLIBS_OVERRIDES) $(OLDS)

.PHONY: cross-flexdll
cross-flexdll: | $(BYTE_BINDIR) $(OPT_BINDIR)
	rm -f $(FLEXDLL_SOURCE_DIR)/flexlink.exe
	$(MAKE) -C $(FLEXDLL_SOURCE_DIR) $(FLEXLINK_BUILD_ENV) \
	  NATDYNLINK=false LINKFLAGS= flexlink.exe support
	$(LN) $(FLEXDLL_SOURCE_DIR)/flexlink.exe flexlink.opt.exe
	$(LN) flexlink.opt.exe flexlink.byte.exe
	cp flexlink.byte.exe $(BYTE_BINDIR)/flexlink
	cd $(BYTE_BINDIR) && $(LN) flexlink flexlink.exe
	cp $(addprefix $(FLEXDLL_SOURCE_DIR)/, $(FLEXDLL_OBJECTS)) $(BYTE_BINDIR)
	cp flexlink.opt.exe $(OPT_BINDIR)/flexlink
	cd $(OPT_BINDIR) && $(LN) flexlink flexlink.exe
	cp $(addprefix $(FLEXDLL_SOURCE_DIR)/, $(FLEXDLL_OBJECTS)) $(OPT_BINDIR)

INSTALL_OVERRIDES=build_ocamldoc=false WITH_DEBUGGER= OCAMLRUN=ocamlrun

.PHONY: installcross
installcross:
	# Create dummy files to keep `install` happy
	touch \
	  $(addprefix toplevel/, \
	    $(foreach ext,cmi cmt cmti cmx, native/nat__dummy__.$(ext)) \
	      all__dummy__.cmx topstart.o native/tophooks.cmi)
	$(LN) `command -v ocamllex` lex/ocamllex.opt$(EXE)
	$(LN) `command -v ocamlyacc` yacc/ocamlyacc.opt$(EXE)
	# Real installation
	$(MAKE) install $(INSTALL_OVERRIDES)
