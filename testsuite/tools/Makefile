#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                 Jeremie Dimino, Jane Street Europe                     *
#*                                                                        *
#*   Copyright 2016 Jane Street Group LLC                                 *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR = ..

ROOTDIR = ../..

include $(ROOTDIR)/Makefile.config
expect_MAIN=expect_test
expect_PROG=$(expect_MAIN)$(EXE)
expect_COMPFLAGS=-I $(OTOPDIR)/parsing -I $(OTOPDIR)/utils \
          -I $(OTOPDIR)/driver -I $(OTOPDIR)/typing -I $(OTOPDIR)/toplevel
expect_LIBRARIES := $(addprefix $(ROOTDIR)/compilerlibs/,\
  ocamlcommon ocamlbytecomp ocamltoplevel)

codegen_INCLUDES=\
  -I $(OTOPDIR)/parsing \
  -I $(OTOPDIR)/utils \
  -I $(OTOPDIR)/typing \
  -I $(OTOPDIR)/middle_end \
  -I $(OTOPDIR)/bytecomp \
  -I $(OTOPDIR)/lambda \
  -I $(OTOPDIR)/asmcomp

codegen_OTHEROBJECTS=\
  $(OTOPDIR)/compilerlibs/ocamlcommon.cma \
  $(OTOPDIR)/compilerlibs/ocamloptcomp.cma

codegen_OBJECTS=parsecmmaux.cmo parsecmm.cmo lexcmm.cmo codegen_main.cmo

codegen_ADD_COMPFLAGS=$(codegen_INCLUDES) -w -40 -g

targets := $(expect_PROG)

ifneq "$(ARCH)" "none"
targets += codegen
ifneq "$(CCOMPTYPE)-$(ARCH)" "msvc-amd64"
# The asmgen tests are not ported to MSVC64 yet
# so do not compile any arch-specific module
targets += asmgen_$(ARCH).$(O)
endif
endif

all: $(targets)

$(expect_PROG): $(expect_LIBRARIES:=.cma) $(expect_MAIN).cmo
	@$(OCAMLC) -linkall -o $@ $^

include $(BASEDIR)/makefiles/Makefile.common

.PHONY: clean
clean: defaultclean
	rm -f $(expect_PROG)
	rm -f codegen parsecmm.ml parsecmm.mli lexcmm.ml

expect_test.cmo: COMPFLAGS=$(expect_COMPFLAGS)

$(codegen_OBJECTS): ADD_COMPFLAGS = $(codegen_ADD_COMPFLAGS)

codegen_main.cmo: parsecmm.cmo

codegen: $(codegen_OBJECTS)
	@$(OCAMLC) $(LINKFLAGS) -o $@ $(codegen_OTHEROBJECTS) $^

parsecmm.mli parsecmm.ml: parsecmm.mly
	@$(OCAMLYACC) -q parsecmm.mly

lexcmm.ml: lexcmm.mll
	@$(OCAMLLEX) -q lexcmm.mll

asmgen_i386.obj: asmgen_i386nt.asm
	@set -o pipefail ; \
	$(ASM) $@ $^ | tail -n +2
