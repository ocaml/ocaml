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

config.ml: config.mlp $(ROOTDIR)/Makefile.config Makefile
	sed $(call SUBST,AFL_INSTRUMENT) \
	    $(call SUBST,ARCH) \
	    $(call SUBST_STRING,ARCMD) \
	    $(call SUBST_STRING,ASM) \
	    $(call SUBST,ASM_CFI_SUPPORTED) \
	    $(call SUBST_STRING,BYTECCLIBS) \
	    $(call SUBST_STRING,CC) \
	    $(call SUBST_STRING,CCOMPTYPE) \
	    $(call SUBST_STRING,OUTPUTOBJ) \
	    $(call SUBST_STRING,EXT_ASM) \
	    $(call SUBST_STRING,EXT_DLL) \
	    $(call SUBST_STRING,EXE) \
	    $(call SUBST_STRING,EXT_LIB) \
	    $(call SUBST_STRING,EXT_OBJ) \
	    $(call SUBST,FLAMBDA) \
	    $(call SUBST,WITH_FLAMBDA_INVARIANTS) \
	    $(call SUBST_STRING,FLEXLINK_FLAGS) \
	    $(call SUBST_QUOTE,FLEXDLL_DIR) \
	    $(call SUBST,HOST) \
	    $(call SUBST_STRING,LIBDIR) \
	    $(call SUBST,LIBUNWIND_AVAILABLE) \
	    $(call SUBST,LIBUNWIND_LINK_FLAGS) \
	    $(call SUBST_STRING,MKDLL) \
	    $(call SUBST_STRING,MKEXE) \
	    $(call SUBST_STRING,FLEXLINK_LDFLAGS) \
	    $(call SUBST_STRING,MKMAINDLL) \
	    $(call SUBST,MODEL) \
	    $(call SUBST_STRING,NATIVECCLIBS) \
	    $(call SUBST_STRING,OCAMLC_CFLAGS) \
	    $(call SUBST_STRING,OCAMLC_CPPFLAGS) \
	    $(call SUBST_STRING,OCAMLOPT_CFLAGS) \
	    $(call SUBST_STRING,OCAMLOPT_CPPFLAGS) \
	    $(call SUBST_STRING,PACKLD) \
	    $(call SUBST,PROFINFO_WIDTH) \
	    $(call SUBST_STRING,RANLIBCMD) \
	    $(call SUBST,FORCE_SAFE_STRING) \
	    $(call SUBST,DEFAULT_SAFE_STRING) \
	    $(call SUBST,WINDOWS_UNICODE) \
	    $(call SUBST,SUPPORTS_SHARED_LIBRARIES) \
	    $(call SUBST,SYSTEM) \
	    $(call SUBST,SYSTHREAD_SUPPORT) \
	    $(call SUBST,TARGET) \
	    $(call SUBST,WITH_FRAME_POINTERS) \
	    $(call SUBST,WITH_PROFINFO) \
	    $(call SUBST,WITH_SPACETIME) \
	    $(call SUBST,ENABLE_CALL_COUNTS) \
	    $(call SUBST,FLAT_FLOAT_ARRAY) \
	    $(call SUBST,FUNCTION_SECTIONS) \
	    $(call SUBST,CC_HAS_DEBUG_PREFIX_MAP) \
	    $(call SUBST,AS_HAS_DEBUG_PREFIX_MAP) \
	    $< > $@

ocaml.ini: $(ROOTDIR)/Makefile.config Makefile
	(echo "afl_instrument=$(AFL_INSTRUMENT)"; \
	 echo "arch=$(ARCH)"; \
	 echo "arcmd='$(ARCMD)'"; \
	 echo "asm='$(ASM)'"; \
	 echo "asm_cfi_supported=$(ASM_CFI_SUPPORTED)"; \
	 echo "bytecclibs='$(BYTECCLIBS)'"; \
	 echo "cc='$(CC)'"; \
	 echo "ccomptype=$(CCOMPTYPE)"; \
	 echo "outputobj='$(OUTPUTOBJ)'"; \
	 echo "ext_asm='$(EXT_ASM)'"; \
	 echo "ext_dll='$(EXT_DLL)'"; \
	 echo "exe='$(EXE)'"; \
	 echo "ext_lib='$(EXT_LIB)'"; \
	 echo "ext_obj='$(EXT_OBJ)'"; \
	 echo "flambda=$(FLAMBDA)"; \
	 echo "with_flambda_invariants=$(WITH_FLAMBDA_INVARIANTS)"; \
	 echo "flexlink_flags='$(FLEXLINK_FLAGS)'"; \
	 echo "flexdll_dir='$(FLEXDLL_DIR)'"; \
	 echo "host=$(HOST)"; \
	 echo "libdir='$(LIBDIR)'"; \
	 echo "libunwind_available=$(LIBUNWIND_AVAILABLE)"; \
	 echo "libunwind_link_flags='$(LIBUNWIND_LINK_FLAGS)'"; \
	 echo "mkdll='$(MKDLL)'"; \
	 echo "mkexe='$(MKEXE)'"; \
	 echo "flexlink_ldflags='$(FLEXLINK_LDFLAGS)'"; \
	 echo "mkmaindll='$(MKMAINDLL)'"; \
	 echo "model=$(MODEL)"; \
	 echo "nativecclibs='$(NATIVECCLIBS)'"; \
	 echo "ocamlc_cflags='$(OCAMLC_CFLAGS)'"; \
	 echo "ocamlc_cppflags='$(OCAMLC_CPPFLAGS)'"; \
	 echo "ocamlopt_cflags='$(OCAMLOPT_CFLAGS)'"; \
	 echo "ocamlopt_cppflags='$(OCAMLOPT_CPPFLAGS)'"; \
	 echo "packld='$(PACKLD)'"; \
	 echo "profinfo_width=$(PROFINFO_WIDTH)"; \
	 echo "ranlibcmd='$(RANLIBCMD)'"; \
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
