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

FLEXLINK_FLAGS ?=

# Escape special characters in the argument string.
# There are four characters that need escaping:
# - backslash and ampersand, which are special in the replacement text
#   of sed's "s" command
# - exclamation mark, which is the delimiter we use for sed's "s" command
# - single quote, which interferes with shell quoting.  We are inside
#   single quotes already, so the proper escape is '\''
#   (close single quotation, insert single quote character,
#    reopen single quotation).
SED_ESCAPE=$(subst ','\'',$(subst !,\!,$(subst &,\&,$(subst \,\\,$1))))

# Escape special characters in an OCaml string literal "..."
# There are two: backslash and double quote.
OCAML_ESCAPE=$(subst ",\",$(subst \,\\,$1))

# SUBST generates the sed substitution for the variable *named* in $1
SUBST=-e 's!%%$1%%!$(call SED_ESCAPE,$($1))!'

# SUBST_STRING does the same, for a variable that occurs between "..."
# in config.mlp.  Thus, backslashes and double quotes must be escaped.
SUBST_STRING=-e 's!%%$1%%!$(call SED_ESCAPE,$(call OCAML_ESCAPE,$($1)))!'

# SUBST_QUOTE does the same, adding OCaml quotes around non-empty strings
#   (see FLEXDLL_DIR which must empty if FLEXDLL_DIR is empty but an OCaml
#    string otherwise)
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
	    $(call SUBST,CC_HAS_DEBUG_PREFIX_MAP) \
	    $(call SUBST,AS_HAS_DEBUG_PREFIX_MAP) \
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
