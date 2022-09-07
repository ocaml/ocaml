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

.NOTPARALLEL:

ROOTDIR = ../..

COMPILERLIBSDIR = $(ROOTDIR)/compilerlibs

RUNTIME_VARIANT ?=

include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

OCAMLC ?= $(BEST_OCAMLC) $(STDLIBFLAGS)
OCAMLOPT ?= $(BEST_OCAMLOPT) $(STDLIBFLAGS)

expect_MAIN=expect_test
expect_PROG=$(expect_MAIN)$(EXE)
expect_DIRS = parsing utils driver typing toplevel
expect_OCAMLFLAGS = $(addprefix -I $(ROOTDIR)/,$(expect_DIRS))
expect_LIBS := $(addprefix $(COMPILERLIBSDIR)/,\
  ocamlcommon ocamlbytecomp ocamltoplevel)

codegen_PROG = codegen$(EXE)
codegen_DIRS = parsing utils typing middle_end bytecomp lambda asmcomp
codegen_OCAMLFLAGS = $(addprefix -I $(ROOTDIR)/, $(codegen_DIRS)) -w +40 -g

codegen_LIBS = $(addprefix $(COMPILERLIBSDIR)/,\
  ocamlcommon ocamloptcomp)

codegen_OBJECTS = $(addsuffix .cmo,\
  parsecmmaux parsecmm lexcmm codegen_main)

tools := $(expect_PROG)

ifeq "$(NATIVE_COMPILER)" "true"
tools += $(codegen_PROG)
ifneq "$(CCOMPTYPE)-$(ARCH)" "msvc-amd64"
# The asmgen tests are not ported to MSVC64 yet
# so do not compile any arch-specific module
tools += asmgen_$(ARCH).$(O)
endif
endif

all: $(tools)

$(expect_PROG): $(expect_LIBS:=.cma) $(expect_MAIN).cmo
	$(OCAMLC) -linkall -o $@ $^

$(expect_PROG): COMPFLAGS = $(expect_OCAMLFLAGS)

$(codegen_PROG): COMPFLAGS = $(codegen_OCAMLFLAGS)

codegen_main.cmo: parsecmm.cmo

$(codegen_PROG): $(codegen_OBJECTS)
	$(OCAMLC) -o $@ $(COMPFLAGS) $(codegen_LIBS:=.cma) $^

parsecmmaux.cmo: parsecmmaux.cmi

lexcmm.cmo: lexcmm.cmi

parsecmm.cmo: parsecmm.cmi

asmgen_i386.obj: asmgen_i386nt.asm
	@set -o pipefail ; \
	$(ASM) $@ $^ | tail -n +2

%.cmi: %.mli
	$(OCAMLC) $(COMPFLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(COMPFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(COMPFLAGS) -c $<

%.$(O): %.S
	$(ASPP) $(OC_ASPPFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -f *.cm* *.o *.obj
	rm -f expect_test expect_test.exe codegen codegen.exe
	rm -f parsecmm.ml parsecmm.mli parsecmm.output lexcmm.ml

.PHONY: distclean
distclean: clean
