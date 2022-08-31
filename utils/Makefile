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

FLEXLINK_FLAGS ?=

# SUBST_QUOTE does the same as SUBST_STRING, adding OCaml quotes around
#   non-empty strings (see FLEXDLL_DIR which must empty if FLEXDLL_DIR is empty
#   but an OCaml string otherwise)
SUBST_QUOTE2=\
  -e 's!%%$1%%!$(if $2,$(call SED_ESCAPE,"$(call OCAML_ESCAPE,$2)"))!'
SUBST_QUOTE=$(call SUBST_QUOTE2,$1,$($1))

prefix_and_quote = $(if $(1), $(2)$(if $(word 2,$(1)),"$(1)",$(1)))

FLEXLINK_LDFLAGS = $(call prefix_and_quote,$(OC_LDFLAGS),-link $(EMPTY))
FLEXLINK_DLL_LDFLAGS = $(call prefix_and_quote,$(OC_DLL_LDFLAGS),-link $(EMPTY))

config_main.ml: \
  config.generated.ml $(ROOTDIR)/Makefile.config Makefile config.common.ml
	cat config.generated.ml config.common.ml > $@

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
