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

ROOTDIR = ..
SUBDIR_NAME = stdlib
# NOTE: it is important that the OCAMLDEP variable is defined *before*
# Makefile.common gets included, so that its local definition here
# take precedence over its general shared definitions in Makefile.common.
OCAMLDEP ?= $(BOOT_OCAMLDEP)

include $(ROOTDIR)/Makefile.common

%.$(O): %.c $(REQUIRED_HEADERS)
	$(V_CC)$(CC) $(OC_CFLAGS) $(CFLAGS) $(OC_CPPFLAGS) $(CPPFLAGS) \
	  $(OUTPUTOBJ)$@ -c $<

# There are three ways the Standard Library is compiled in bytecode:
# 1. During coldstart
#      - using ../boot/ocamlc which runs on ../boot/ocamlrun
# 2. During coreall (via library),
#      - using ../ocamlc which runs on ../boot/ocamlrun
# 3. During coreboot (via library-cross),
#      - using ../ocamlc which at that point runs on ../runtime/ocamlrun
# If $(USE_BOOT_OCAMLC) is non-empty, we select case 1 and use $(BOOT_OCAMLC).
# Otherwise, we use $(OCAMLRUN) ../ocamlc, with $(OCAMLRUN) being
# ../boot/ocamlrun by default, but able to overridden by library-cross to
# ../runtime/ocamlrun.
USE_BOOT_OCAMLC ?=

ifeq "$(USE_BOOT_OCAMLC)" ""
CAMLC = $(OCAMLRUN) $(ROOTDIR)/ocamlc$(EXE)
else
CAMLC = $(BOOT_OCAMLC)
endif
COMPFLAGS = -strict-sequence -absname -w +a-4-9-41-42-44-45-48 \
            -g -warn-error +A -bin-annot -nostdlib -principal
ifeq "$(FLAMBDA)" "true"
OPTCOMPFLAGS += -O3
endif
OPTCOMPILER=$(ROOTDIR)/ocamlopt$(EXE)
CAMLOPT=$(OCAMLRUN) $(OPTCOMPILER)

# At present, only META is installed to the package directory
LIBNAME = stdlib

include StdlibModules

OBJS=$(addsuffix .cmo,$(STDLIB_MODULES))
NOSTDLIB= camlinternalFormatBasics.cmo stdlib.cmo
OTHERS=$(filter-out $(NOSTDLIB),$(OBJS))

.PHONY: all
all: stdlib.cma std_exit.cmo $(HEADER_NAME)

.PHONY: allopt opt.opt # allopt and opt.opt are synonyms
allopt: stdlib.cmxa std_exit.cmx
opt.opt: allopt

.PHONY: install
install::
	$(call INSTALL_ITEMS, stdlib.cma std_exit.cmo *.cmi, lib)
	$(call INSTALL_ITEMS, META, lib, $(LIBNAME))
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(call INSTALL_ITEMS, *.cmt *.cmti *.mli *.ml *.ml.in, lib)
endif
	$(call INSTALL_ITEMS, $(HEADER_NAME), lib)

.PHONY: installopt
installopt: installopt-default

.PHONY: installopt-default
installopt-default:
	$(call INSTALL_ITEMS, stdlib.cmxa stdlib.$(A) std_exit.$(O) *.cmx, lib)

MANGLING = $(filter true,$(SUFFIXING))
runtime-launch-info: tmpheader.exe
	@{ printf '$(if $(MANGLING),$(ZINC_RUNTIME_ID_HI),\000)'; \
	   cat $^; } > $@

# The mingw-w64 and MSVC versions of tmpheader.exe are linked with special flags
# to reduce their size (considerably). In particular, the entry point is
# overridden, which prevents the linking of crt0.
ifeq "$(TOOLCHAIN)" "mingw"
# mingw-w64: optimise header.o for space and remove all unused sections during
# linking.
header.o: OC_CFLAGS += -Os
ifeq "$(SYSTEM)" "mingw"
  ENTRYPOINT = _wmainCRTStartup
else
  ENTRYPOINT = wmainCRTStartup
endif
# Certainly for GCC, -nostdlib automatically adds -nostartfiles (clang's
# documentation is not explicit on this point, but it would be unusual for clang
# and GCC to differ). --gc-sections removes unreferenced code and data sections.
tmpheader.exe: OC_LDFLAGS += \
  -nostdlib -nostartfiles -Wl,--gc-sections -Wl,-e,$(ENTRYPOINT)
HEADERLIBS = -lkernel32
else ifeq "$(TOOLCHAIN)" "msvc"
# MSVC: Optimise header.obj for space. cl doesn't have an equivalent of
# -nostdlib, however it also doesn't have default libraries in the same way as
# GCC - the equivalent of crt0.o is linked because it provides the entry point
# symbol, rather than by being explicitly added to the linking command line.
# /GS- disables compilation of runtime security checks and /NOCOFFGRPINFO is an
# undocumented linker flag which removes the debug directory from the PE+ image.
header.obj: OC_CFLAGS += /GS- /Os
tmpheader.exe: OC_LDFLAGS += /NOCOFFGRPINFO /subsystem:console
HEADERLIBS = kernel32.lib
else
HEADERLIBS =
endif

.INTERMEDIATE: tmpheader.exe
tmpheader.exe: header.$(O)
	$(V_MKEXE)$(call MKEXE_VIA_CC,$@,$^ $(HEADERLIBS))
# Do not strip the header produced by cl
ifneq "$(TOOLCHAIN)" "msvc"
	$(STRIP) $@
endif

stdlib.cma: $(OBJS)
	$(V_LINKC)$(CAMLC) -a -o $@ $^

stdlib.cmxa: $(OBJS:.cmo=.cmx)
	$(V_LINKOPT)$(CAMLOPT) -a -o $@ $^

.PHONY: distclean
distclean: clean
	rm -f sys.ml META

.PHONY: clean
clean::
	rm -f $(HEADER_NAME)

export AWK

%.cmi: %.mli
	$(V_OCAMLC)$(CAMLC) $(COMPFLAGS) $(shell ./Compflags $@) -c $<

# The dependency on the .mli file is in .depend (since stdlib__Foo.cmi
# depends on stdlib__foo.mli)
stdlib__%.cmi:
	$(V_OCAMLC)$(CAMLC) $(COMPFLAGS) $(shell ./Compflags $@) \
	         -o $@ -c $(filter %.mli, $^)

%.cmo: %.ml
	$(V_OCAMLC)$(CAMLC) $(COMPFLAGS) $(shell ./Compflags $@) -c $<

# The dependency on the .ml file is in .depend (since stdlib__Foo.cmo
# depends on stdlib__foo.ml)
stdlib__%.cmo:
	$(V_OCAMLC)$(CAMLC) $(COMPFLAGS) $(shell ./Compflags $@) \
	         -o $@ -c $(filter %.ml, $^)

%.cmx: %.ml
	$(V_OCAMLOPT)$(CAMLOPT) $(COMPFLAGS) $(OPTCOMPFLAGS) $(shell ./Compflags $@) -c $<

# The dependency on the .ml file is in .depend (since stdlib__Foo.cmx
# depends on stdlib__foo.ml)
stdlib__%.cmx:
	$(V_OCAMLOPT)$(CAMLOPT) $(COMPFLAGS) $(OPTCOMPFLAGS) $(shell ./Compflags $@) \
	           -o $@ -c $(filter %.ml, $^)

# Dependencies on the compiler
COMPILER_DEPS=$(filter-out $(OCAMLRUN), $(CAMLC))
$(OBJS) std_exit.cmo: $(COMPILER_DEPS)
$(OBJS:.cmo=.cmi) std_exit.cmi: $(COMPILER_DEPS)
$(OBJS:.cmo=.cmx) std_exit.cmx: $(OPTCOMPILER)

# Dependencies on Stdlib (not tracked by ocamlc -depend)

$(OTHERS) std_exit.cmo: stdlib.cmi
$(OTHERS:.cmo=.cmi) std_exit.cmi: stdlib.cmi
$(OBJS:.cmo=.cmx) std_exit.cmx: stdlib.cmi
$(OTHERS:.cmo=.cmx) std_exit.cmx: stdlib.cmx

clean::
	rm -f *.cm* *.o *.obj *.a *.lib *.odoc

.PHONY: .depend
include .depend

STDLIB_NAMESPACE_MODULES = $(subst $(SPACE),|,$(STDLIB_PREFIXED_MODULES))

GNUISH_SED = \
  $(if $(filter X,$(shell echo x | $(SED) -E -e 's/./\u&/' 2>/dev/null)),\
       $(SED),$(error GNU sed is needed for make depend))

.PHONY: depend
depend:
	$(V_OCAMLDEP){ \
	  $(OCAMLDEP_CMD) $(filter-out stdlib.%,$(wildcard *.mli *.ml)); \
	  $(OCAMLDEP_CMD) -pp "$(AWK) -f ./remove_module_aliases.awk" \
	  stdlib.ml stdlib.mli; \
	} | \
	$(GNUISH_SED) -E \
	-e 's/^(${STDLIB_NAMESPACE_MODULES})(\.[^i]*)(i?) :/\1\2\3 : \1.ml\3/' \
	-e 's#(^| )(${STDLIB_NAMESPACE_MODULES})[.]#\1stdlib__\u\2.#' \
	> .$@
