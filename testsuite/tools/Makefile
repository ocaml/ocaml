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

expect_MAIN=expect_test
expect_PROG=$(expect_MAIN)$(EXE)
expect_DIRS = parsing utils driver typing toplevel
expect_OCAMLFLAGS = $(addprefix -I $(ROOTDIR)/,$(expect_DIRS))
expect_LIBS := $(addsuffix .cma,$(addprefix $(COMPILERLIBSDIR)/,\
  ocamlcommon ocamlbytecomp ocamltoplevel))

codegen_PROG = codegen$(EXE)
codegen_DIRS = parsing utils typing middle_end bytecomp lambda asmcomp
codegen_OCAMLFLAGS = $(addprefix -I $(ROOTDIR)/, $(codegen_DIRS)) -w +40 -g

codegen_LIBS = $(addsuffix .cma, $(addprefix $(COMPILERLIBSDIR)/,\
  ocamlcommon ocamloptcomp))

codegen_CMI_FILES = $(addsuffix .cmi,\
  parsecmmaux parsecmm lexcmm)
codegen_CMO_FILES = $(addsuffix .cmo,\
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

.PHONY: all
all: $(tools)

$(expect_PROG): $(expect_LIBS) $(expect_MAIN).cmo
	$(OCAMLC) -linkall -o $@ $^

$(expect_PROG): COMPFLAGS = $(expect_OCAMLFLAGS)

$(codegen_PROG): COMPFLAGS = $(codegen_OCAMLFLAGS)

codegen_main.cmo: parsecmm.cmo

$(codegen_PROG): $(codegen_LIBS) $(codegen_CMO_FILES)
	$(OCAMLC) -o $@ $(COMPFLAGS) $^

parsecmmaux.cmo: parsecmmaux.cmi

lexcmm.cmo: lexcmm.cmi

parsecmm.cmo: parsecmm.cmi

expect_test.cmo: $(expect_LIBS)

$(codegen_CMI_FILES) $(codegen_CMO_FILES): $(codegen_LIBS)

%.cmi: %.mli
	$(OCAMLC) $(COMPFLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(COMPFLAGS) -c $<

%.$(O): %.S
	$(ASPP) $(OC_ASPPFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -f *.cm* *.o *.obj
	rm -f expect_test expect_test.exe codegen codegen.exe
	rm -f parsecmm.ml parsecmm.mli parsecmm.output lexcmm.ml

.PHONY: distclean
distclean: clean
