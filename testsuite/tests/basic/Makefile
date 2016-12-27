#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                 Xavier Clerc, SED, INRIA Rocquencourt                  *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

all: pr6322.ml check

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.several
include $(BASEDIR)/makefiles/Makefile.common

GENERATED_SOURCES=pr6322.ml *.safe-string

pr6322.ml: $(SAFE_STRING).safe-string
ifeq ($(SAFE_STRING),false)
	@cat pr6322.ml.in > $@
else
	@echo "Printf.printf \"PR#6322=Ok\\n%!\"" > $@
endif

%.safe-string:
	@rm -f pr6322.ml
	@touch $@
