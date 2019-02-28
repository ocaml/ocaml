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

include $(ROOTDIR)/Makefile.config

ifeq "$(UNIX_OR_WIN32)" "win32"
ifeq "$(wildcard $(ROOTDIR)/flexdll/Makefile)" ""
  FLEXDLL_DIR =
else
  FLEXDLL_DIR = $(if $(wildcard $(ROOTDIR)/flexdll/flexdll_*.$(O)),+flexdll)
endif
else
  FLEXDLL_DIR =
endif

# SUBST generates the sed substitution for the variable *named* in $1
# SUBST_QUOTE does the same, adding double-quotes around non-empty strings
#   (see FLEXDLL_DIR which must empty if FLEXDLL_DIR is empty but an OCaml
#    string otherwise)
SUBST_ESCAPE=$(subst ",\\",$(subst \,\\,$(if $2,$2,$($1))))
SUBST=-e 's|%%$1%%|$(call SUBST_ESCAPE,$1,$2)|'
SUBST_QUOTE2=-e 's|%%$1%%|$(if $2,"$2")|'
SUBST_QUOTE=$(call SUBST_QUOTE2,$1,$(call SUBST_ESCAPE,$1,$2))
FLEXLINK_LDFLAGS=$(if $(OC_LDFLAGS), -link "$(OC_LDFLAGS)")
config.ml: config.mlp $(ROOTDIR)/Makefile.config Makefile
	sed $(call SUBST,AFL_INSTRUMENT) \
	    $(call SUBST,ARCH) \
	    $(call SUBST,ARCMD) \
	    $(call SUBST,ASM) \
	    $(call SUBST,ASM_CFI_SUPPORTED) \
	    $(call SUBST,BYTECCLIBS) \
	    $(call SUBST,CC) \
	    $(call SUBST,CCOMPTYPE) \
	    $(call SUBST,CC_PROFILE) \
	    $(call SUBST,OUTPUTOBJ) \
	    $(call SUBST,EXT_ASM) \
	    $(call SUBST,EXT_DLL) \
	    $(call SUBST,EXE) \
	    $(call SUBST,EXT_LIB) \
	    $(call SUBST,EXT_OBJ) \
	    $(call SUBST,FLAMBDA) \
	    $(call SUBST,WITH_FLAMBDA_INVARIANTS) \
	    $(call SUBST,FLEXLINK_FLAGS) \
	    $(call SUBST_QUOTE,FLEXDLL_DIR) \
	    $(call SUBST,HOST) \
	    $(call SUBST,LIBDIR) \
	    $(call SUBST,LIBUNWIND_AVAILABLE) \
	    $(call SUBST,LIBUNWIND_LINK_FLAGS) \
	    $(call SUBST,MKDLL) \
	    $(call SUBST,MKEXE) \
	    $(call SUBST,FLEXLINK_LDFLAGS) \
	    $(call SUBST,MKMAINDLL) \
	    $(call SUBST,MODEL) \
	    $(call SUBST,NATIVECCLIBS) \
	    $(call SUBST,OCAMLC_CFLAGS) \
	    $(call SUBST,OCAMLC_CPPFLAGS) \
	    $(call SUBST,OCAMLOPT_CFLAGS) \
	    $(call SUBST,OCAMLOPT_CPPFLAGS) \
	    $(call SUBST,PACKLD) \
	    $(call SUBST,PROFILING) \
	    $(call SUBST,PROFINFO_WIDTH) \
	    $(call SUBST,RANLIBCMD) \
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
	    $(call SUBST,CC_HAS_DEBUG_PREFIX_MAP) \
	    $(call SUBST,AS_HAS_DEBUG_PREFIX_MAP) \
	    $< > $@
