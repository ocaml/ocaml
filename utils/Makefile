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

# The Makefile for generating the configuration file

ROOTDIR = ..

include $(ROOTDIR)/Makefile.common

ifeq "$(UNIX_OR_WIN32)" "win32"
ifeq "$(wildcard $(ROOTDIR)/flexdll/Makefile)" ""
  FLEXDLL_DIR =
else
  FLEXDLL_DIR = $(if $(wildcard $(ROOTDIR)/flexdll/flexdll_*.$(O)),+flexdll)
endif
else
  FLEXDLL_DIR =
endif

FLEXLINK_FLAGS ?=

# SUBST_QUOTE does the same as SUBST_STRING, adding OCaml quotes around
#   non-empty strings (see FLEXDLL_DIR which must empty if FLEXDLL_DIR is empty
#   but an OCaml string otherwise)
SUBST_QUOTE2=\
  -e 's!%%$1%%!$(if $2,$(call SED_ESCAPE,"$(call OCAML_ESCAPE,$2)"))!'
SUBST_QUOTE=$(call SUBST_QUOTE2,$1,$($1))

FLEXLINK_LDFLAGS=$(if $(OC_LDFLAGS), -link "$(OC_LDFLAGS)")

config.ml: config.mlp ocaml.ini $(ROOTDIR)/Makefile.config Makefile
	(echo "let builtin_config = {|#"; \
	 cat ocaml.ini; \
	 echo "|}"; \
	 cat $<) > $@

ocaml.ini: $(ROOTDIR)/Makefile.config Makefile
	(echo "afl_instrument=$(AFL_INSTRUMENT)"; \
	 echo "arch=$(ARCH)"; \
	 echo "arcmd=$(ARCMD)"; \
	 echo "asm=$(ASM)"; \
	 echo "asm_cfi_supported=$(ASM_CFI_SUPPORTED)"; \
	 echo "bytecclibs=$(BYTECCLIBS)"; \
	 echo "cc=$(CC)"; \
	 echo "ccomptype=$(CCOMPTYPE)"; \
	 echo "outputobj=$(OUTPUTOBJ)"; \
	 echo "ext_asm=$(EXT_ASM)"; \
	 echo "ext_dll=$(EXT_DLL)"; \
	 echo "exe=$(EXE)"; \
	 echo "ext_lib=$(EXT_LIB)"; \
	 echo "ext_obj=$(EXT_OBJ)"; \
	 echo "flambda=$(FLAMBDA)"; \
	 echo "with_flambda_invariants=$(WITH_FLAMBDA_INVARIANTS)"; \
	 echo "flexlink_flags=$(FLEXLINK_FLAGS)"; \
	 echo "flexdll_dir=$(FLEXDLL_DIR)"; \
	 echo "host=$(HOST)"; \
	 echo "libdir=$(LIBDIR)"; \
	 echo "libunwind_available=$(LIBUNWIND_AVAILABLE)"; \
	 echo "libunwind_link_flags=$(LIBUNWIND_LINK_FLAGS)"; \
	 echo "mkdll=$(MKDLL)"; \
	 echo "mkexe=$(MKEXE)"; \
	 echo "flexlink_ldflags=$(FLEXLINK_LDFLAGS)"; \
	 echo "mkmaindll=$(MKMAINDLL)"; \
	 echo "model=$(MODEL)"; \
	 echo "nativecclibs=$(NATIVECCLIBS)"; \
	 echo "ocamlc_cflags=$(OCAMLC_CFLAGS)"; \
	 echo "ocamlc_cppflags=$(OCAMLC_CPPFLAGS)"; \
	 echo "ocamlopt_cflags=$(OCAMLOPT_CFLAGS)"; \
	 echo "ocamlopt_cppflags=$(OCAMLOPT_CPPFLAGS)"; \
	 echo "packld=$(PACKLD)"; \
	 echo "profinfo_width=$(PROFINFO_WIDTH)"; \
	 echo "ranlibcmd=$(RANLIBCMD)"; \
	 echo "force_safe_string=$(FORCE_SAFE_STRING)"; \
	 echo "default_safe_string=$(DEFAULT_SAFE_STRING)"; \
	 echo "windows_unicode=$(WINDOWS_UNICODE)"; \
	 echo "supports_shared_libraries=$(SUPPORTS_SHARED_LIBRARIES)"; \
	 echo "system=$(SYSTEM)"; \
	 echo "systhread_support=$(SYSTHREAD_SUPPORT)"; \
	 echo "target=$(TARGET)"; \
	 echo "with_frame_pointers=$(WITH_FRAME_POINTERS)"; \
	 echo "with_profinfo=$(WITH_PROFINFO)"; \
	 echo "with_spacetime=$(WITH_SPACETIME)"; \
	 echo "enable_call_counts=$(ENABLE_CALL_COUNTS)"; \
	 echo "flat_float_array=$(FLAT_FLOAT_ARRAY)"; \
	 echo "function_sections=$(FUNCTION_SECTIONS)"; \
	 echo "cc_has_debug_prefix_map=$(CC_HAS_DEBUG_PREFIX_MAP)"; \
	 echo "as_has_debug_prefix_map=$(AS_HAS_DEBUG_PREFIX_MAP)";) \
	> $@

# Test for the substitution functions above

ALLCHARS= \
  !"\#\$\%&'()*+,-./ \
  0123456789:;<=>? \
  @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_ \
  `abcdefghijklmnopqrstuvwxyz{|}~

TMPFILE=testdata.tmp
TMPSCRIPT=ocamlscript.tmp

test-subst:
	$(file >$(TMPFILE),$(ALLCHARS))
	echo '%%ALLCHARS%%' | sed $(call SUBST,ALLCHARS) | cmp $(TMPFILE) -
	@rm $(TMPFILE)
	@echo "Test passed"

# This test assumes there is a working OCaml in the path

test-subst-string:
	$(file >$(TMPFILE),$(ALLCHARS))
	echo 'print_string "%%ALLCHARS%%"; print_newline();;' \
        | sed $(call SUBST_STRING,ALLCHARS) > $(TMPSCRIPT) && \
        ocaml $(TMPSCRIPT) | cmp $(TMPFILE) -
	@rm $(TMPFILE) $(TMPSCRIPT)
	@echo "Test passed"
