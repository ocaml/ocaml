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
# NOTE: it is important that the OCAMLDEP variable is defined *before*
# Makefile.common gets included, so that its local definition here
# take precedence over its general shared definitions in Makefile.common.
OCAMLDEP ?= $(BOOT_OCAMLDEP)

include $(ROOTDIR)/Makefile.common

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

include StdlibModules

OBJS=$(addsuffix .cmo,$(STDLIB_MODULES))
NOSTDLIB= camlinternalFormatBasics.cmo stdlib.cmo
OTHERS=$(filter-out $(NOSTDLIB),$(OBJS))

.PHONY: all
all: stdlib.cma std_exit.cmo $(HEADER_NAME) target_$(HEADER_NAME)

.PHONY: allopt opt.opt # allopt and opt.opt are synonyms
allopt: stdlib.cmxa std_exit.cmx
opt.opt: allopt

byte/dynlink.ml: byte/dynlink.mli \
  dynlink_common.ml.in \
  $(ROOTDIR)/Makefile.build_config $(ROOTDIR)/Makefile.config \
  $(ROOTDIR)/file_formats/cmo_format.mli \
  byte/dynlink_symtable.ml.in byte/dynlink.ml.in
	$(V_GEN){ \
	  echo 'module DC = struct'; \
	  cat dynlink_common.ml.in; \
	  echo 'end'; \
	  echo 'module Config = struct let ext_dll = "$(EXT_DLL)" end'; \
	  echo 'let cmo_magic_number = $(CMO_MAGIC_TOKEN)'; \
	  echo 'let cma_magic_number = $(CMA_MAGIC_TOKEN)'; \
	  echo '[@@@ocaml.warning "-37-69"]'; \
	  echo '#1 "$(ROOTDIR)/file_formats/cmo_format.mli"'; \
	  cat $(ROOTDIR)/file_formats/cmo_format.mli; \
	  echo '[@@@ocaml.warning "+37+69"]'; \
	  echo 'module Symtable = struct'; \
	  cat byte/dynlink_symtable.ml.in; \
	  echo 'end'; \
	  cat byte/dynlink.ml.in; \
	} > $@

native/dynlink.ml: native/dynlink.mli \
  dynlink_common.ml.in \
  $(ROOTDIR)/Makefile.build_config \
  $(ROOTDIR)/file_formats/cmxs_format.mli \
  native/dynlink.ml.in
	$(V_GEN){ \
	  echo 'module DC = struct'; \
	  cat dynlink_common.ml.in; \
	  echo 'end'; \
	  echo 'let cmxs_magic_number = $(CMXS_MAGIC_TOKEN)'; \
	  echo '#1 "$(ROOTDIR)/file_formats/cmxs_format.mli"'; \
	  cat $(ROOTDIR)/file_formats/cmxs_format.mli; \
	  cat native/dynlink.ml.in; \
	} > $@

%/dynlink.mli: dynlink.mli
	cd $*; $(LN) ../$^ $^

INSTALL_STDLIB_META_DIR=$(DESTDIR)$(LIBDIR)/stdlib

.PHONY: install
install::
	$(INSTALL_DATA) \
	  stdlib.cma std_exit.cmo *.cmi "$(INSTALL_LIBDIR)"
	$(MKDIR) "$(INSTALL_STDLIB_META_DIR)"
	$(INSTALL_DATA) META "$(INSTALL_STDLIB_META_DIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  *.cmt *.cmti *.mli *.ml *.ml.in \
	  "$(INSTALL_LIBDIR)"
endif
	$(INSTALL_DATA) target_$(HEADER_NAME) "$(INSTALL_LIBDIR)/$(HEADER_NAME)"

.PHONY: installopt
installopt: installopt-default

.PHONY: installopt-default
installopt-default:
	$(INSTALL_DATA) \
	  stdlib.cmxa stdlib.$(A) std_exit.$(O) *.cmx \
	  "$(INSTALL_LIBDIR)"

ifeq "$(UNIX_OR_WIN32)" "unix"
HEADERPROGRAM = header
else # Windows
HEADERPROGRAM = headernt
endif

%-launch-info: %.info tmpheader.exe
	@cat $^ >> $@

.INTERMEDIATE: tmpheader.exe
tmpheader.exe: $(HEADERPROGRAM).$(O)
	$(V_MKEXE)$(call MKEXE_VIA_CC,$@,$^)
	$(STRIP) $@

stdlib.cma: $(OBJS)
	$(V_LINKC)$(CAMLC) -a -o $@ $^

stdlib.cmxa: $(OBJS:.cmo=.cmx)
	$(V_LINKOPT)$(CAMLOPT) -a -o $@ $^

.PHONY: distclean
distclean: clean
	rm -f sys.ml META runtime.info target_runtime.info

.PHONY: clean
clean::
	rm -f $(HEADER_NAME) target_$(HEADER_NAME) \
	  byte/dynlink.mli byte/dynlink.ml native/dynlink.mli native/dynlink.ml

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

include .depend

NAMESPACE_MODULES = $(subst $(SPACE),|,$(STDLIB_PREFIXED_MODULES))

GNUISH_SED = \
  $(if $(filter X,$(shell echo x | $(SED) -E -e 's/./\u&/' 2>/dev/null)),\
       $(SED),$(error GNU sed is needed for make depend))

.PHONY: depend
depend: byte/dynlink.ml native/dynlink.ml
	$(V_OCAMLDEP){ \
	  $(OCAMLDEP_CMD) $(filter-out stdlib.%,$(wildcard *.mli *.ml)); \
	  $(OCAMLDEP_CMD) -bytecode byte/dynlink.ml | sed -e 's| byte/| |'; \
	  $(OCAMLDEP_CMD) -native native/dynlink.ml | sed -e 's| native/| |'; \
	  $(OCAMLDEP_CMD) -pp "$(AWK) -f ./remove_module_aliases.awk" \
	  stdlib.ml stdlib.mli ; \
	} | \
	$(GNUISH_SED) -E \
	-e 's/^([^\/]+\/)?(${NAMESPACE_MODULES})(\.[^i]*)(i?) :/\2\3\4 : \1\2.ml\4/' \
	-e 's#(^| )(${NAMESPACE_MODULES})[.]#\1stdlib__\u\2.#' \
	> .$@
