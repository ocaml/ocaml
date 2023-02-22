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

ifeq "$(BOOTSTRAPPING_FLEXDLL)" "false"
  FLEXDLL_DIR =
else
  FLEXDLL_DIR = +flexdll
endif

FLEXLINK_FLAGS ?=

# SUBST_QUOTE does the same as SUBST_STRING, adding OCaml quotes around
#   non-empty strings (see FLEXDLL_DIR which must empty if FLEXDLL_DIR is empty
#   but an OCaml string otherwise)
SUBST_QUOTE2=\
  -e 's!%%$1%%!$(if $2,$(call SED_ESCAPE,"$(call OCAML_ESCAPE,$2)"))!'
SUBST_QUOTE=$(call SUBST_QUOTE2,$1,$($1))

prefix_and_quote = $(if $(1), $(2)$(if $(word 2,$(1)),"$(1)",$(1)))

FLEXLINK_LDFLAGS = \
  $(call prefix_and_quote,$(OC_LDFLAGS),-link $(EMPTY)) $(MKEXE_EXTRA_FLAGS)
FLEXLINK_DLL_LDFLAGS = $(call prefix_and_quote,$(OC_DLL_LDFLAGS),-link $(EMPTY))

config_main.ml: config.mlp $(ROOTDIR)/Makefile.config Makefile config.common.ml
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
	    $(call SUBST,WITH_CMM_INVARIANTS) \
	    $(call SUBST_STRING,FLEXLINK_FLAGS) \
	    $(call SUBST_QUOTE,FLEXDLL_DIR) \
	    $(call SUBST_STRING,FLEXDLL_CHAIN) \
	    $(call SUBST,HOST) \
	    $(call SUBST_STRING,BINDIR) \
	    $(call SUBST_STRING,LIBDIR) \
	    $(call SUBST_STRING,MKDLL_EXP) \
	    $(call SUBST_STRING,MKEXE_EXP) \
	    $(call SUBST_STRING,FLEXLINK_LDFLAGS) \
	    $(call SUBST_STRING,FLEXLINK_DLL_LDFLAGS) \
	    $(call SUBST_STRING,MKMAINDLL) \
	    $(call SUBST,MODEL) \
	    $(call SUBST_STRING,NATIVECCLIBS) \
	    $(call SUBST_STRING,OCAMLC_CFLAGS) \
	    $(call SUBST_STRING,OCAMLC_CPPFLAGS) \
	    $(call SUBST_STRING,OCAMLOPT_CFLAGS) \
	    $(call SUBST_STRING,OCAMLOPT_CPPFLAGS) \
	    $(call SUBST_STRING,PACKLD) \
	    $(call SUBST,PROFINFO_WIDTH) \
	    $(call SUBST_STRING,RPATH) \
	    $(call SUBST_STRING,MKSHAREDLIBRPATH) \
	    $(call SUBST,WINDOWS_UNICODE) \
	    $(call SUBST,SUPPORTS_SHARED_LIBRARIES) \
	    $(call SUBST,SYSTEM) \
	    $(call SUBST,SYSTHREAD_SUPPORT) \
	    $(call SUBST,TARGET) \
	    $(call SUBST,WITH_FRAME_POINTERS) \
	    $(call SUBST,WITH_PROFINFO) \
	    $(call SUBST,FLAT_FLOAT_ARRAY) \
	    $(call SUBST,FUNCTION_SECTIONS) \
	    $(call SUBST,CC_HAS_DEBUG_PREFIX_MAP) \
	    $(call SUBST,AS_HAS_DEBUG_PREFIX_MAP) \
	    $< > $@
	cat config.common.ml >> $@

config_boot.ml: config.fixed.ml config.common.ml Makefile
	cat config.fixed.ml config.common.ml > $@

config.ml: config_$(if $(filter true,$(IN_COREBOOT_CYCLE)),boot,main).ml
	cp $^ $@

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
