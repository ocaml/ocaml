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
SUBST_QUOTE=$(call SUBST_QUOTE2,$1,$(if $(2),$(2),$($1)))

CONFIG_VALUE = \
  $(call $(1),$(2),$(if $(filter true,$(IN_COREBOOT_CYCLE)),$(if $(3),$(3),\
                                      <boot compiler>)))

FLEXLINK_LDFLAGS=$(if $(OC_LDFLAGS), -link "$(OC_LDFLAGS)")
FLEXLINK_DLL_LDFLAGS=$(if $(OC_DLL_LDFLAGS), -link "$(OC_DLL_LDFLAGS)")

IN_COREBOOT_CYCLE?=false

config.ml: config.mlp $(ROOTDIR)/Makefile.config Makefile
	sed $(call CONFIG_VALUE,SUBST,AFL_INSTRUMENT,false) \
	    $(call CONFIG_VALUE,SUBST,ARCH,none) \
	    $(call CONFIG_VALUE,SUBST_STRING,ARCMD) \
	    $(call CONFIG_VALUE,SUBST_STRING,ASM) \
	    $(call CONFIG_VALUE,SUBST,ASM_CFI_SUPPORTED,false) \
	    $(call CONFIG_VALUE,SUBST_STRING,BYTECCLIBS) \
	    $(call CONFIG_VALUE,SUBST_STRING,CC) \
	    $(call CONFIG_VALUE,SUBST_STRING,CCOMPTYPE) \
	    $(call CONFIG_VALUE,SUBST_STRING,OUTPUTOBJ) \
	    $(call CONFIG_VALUE,SUBST_STRING,EXT_ASM) \
	    $(call CONFIG_VALUE,SUBST_STRING,EXT_DLL) \
	    $(call CONFIG_VALUE,SUBST_STRING,EXE) \
	    $(call CONFIG_VALUE,SUBST_STRING,EXT_LIB) \
	    $(call CONFIG_VALUE,SUBST_STRING,EXT_OBJ) \
	    $(call CONFIG_VALUE,SUBST,FLAMBDA,false) \
	    $(call CONFIG_VALUE,SUBST,WITH_FLAMBDA_INVARIANTS,false) \
	    $(call CONFIG_VALUE,SUBST,WITH_CMM_INVARIANTS,false) \
	    $(call CONFIG_VALUE,SUBST_STRING,FLEXLINK_FLAGS) \
	    $(call CONFIG_VALUE,SUBST_QUOTE,FLEXDLL_DIR) \
	    $(call CONFIG_VALUE,SUBST,HOST) \
	    $(call CONFIG_VALUE,SUBST_STRING,BINDIR) \
	    $(call CONFIG_VALUE,SUBST_STRING,LIBDIR) \
	    $(call CONFIG_VALUE,SUBST_STRING,MKDLL) \
	    $(call CONFIG_VALUE,SUBST_STRING,MKEXE) \
	    $(call CONFIG_VALUE,SUBST_STRING,FLEXLINK_LDFLAGS) \
	    $(call CONFIG_VALUE,SUBST_STRING,FLEXLINK_DLL_LDFLAGS) \
	    $(call CONFIG_VALUE,SUBST_STRING,MKMAINDLL) \
	    $(call CONFIG_VALUE,SUBST,MODEL,default) \
	    $(call CONFIG_VALUE,SUBST_STRING,NATIVECCLIBS) \
	    $(call CONFIG_VALUE,SUBST_STRING,OCAMLC_CFLAGS) \
	    $(call CONFIG_VALUE,SUBST_STRING,OCAMLC_CPPFLAGS) \
	    $(call CONFIG_VALUE,SUBST_STRING,OCAMLOPT_CFLAGS) \
	    $(call CONFIG_VALUE,SUBST_STRING,OCAMLOPT_CPPFLAGS) \
	    $(call CONFIG_VALUE,SUBST_STRING,PACKLD) \
	    $(call CONFIG_VALUE,SUBST,PROFINFO_WIDTH,0) \
	    $(call CONFIG_VALUE,SUBST_STRING,RPATH) \
	    $(call CONFIG_VALUE,SUBST_STRING,MKSHAREDLIBRPATH) \
	    $(call CONFIG_VALUE,SUBST,WINDOWS_UNICODE,0) \
	    $(call CONFIG_VALUE,SUBST,SUPPORTS_SHARED_LIBRARIES,false) \
	    $(call CONFIG_VALUE,SUBST,SYSTEM,unknown) \
	    $(call CONFIG_VALUE,SUBST,SYSTHREAD_SUPPORT,false) \
	    $(call CONFIG_VALUE,SUBST,TARGET) \
	    $(call CONFIG_VALUE,SUBST,WITH_FRAME_POINTERS,false) \
	    $(call CONFIG_VALUE,SUBST,WITH_PROFINFO,false) \
	    $(call CONFIG_VALUE,SUBST,FLAT_FLOAT_ARRAY,true) \
	    $(call CONFIG_VALUE,SUBST,FUNCTION_SECTIONS,false) \
	    $(call CONFIG_VALUE,SUBST,CC_HAS_DEBUG_PREFIX_MAP,false) \
	    $(call CONFIG_VALUE,SUBST,AS_HAS_DEBUG_PREFIX_MAP,false) \
	    $(call CONFIG_VALUE,SUBST,FORCE_INSTRUMENTED_RUNTIME,false) \
	    $(call CONFIG_VALUE,SUBST,IN_COREBOOT_CYCLE,true) \
	    $< > $@

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
