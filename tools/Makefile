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

ROOTDIR = ..
# NOTE: it is important that OCAMLLEX is defined *before* Makefile.common
# gets included, so that its definition here takes precedence
# over the one there.
OCAMLLEX ?= $(BOOT_OCAMLLEX)
include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

TOOLS_BYTECODE_TARGETS = \
  ocamldep ocamlprof ocamlcp ocamloptp ocamlmklib  \
  ocamlmktop ocamlcmt dumpobj ocamlobjinfo \
  primreq stripdebug cmpbyt
TOOLS_NATIVE_TARGETS = $(TOOLS_BYTECODE_TARGETS:=.opt)

TOOLS_TO_INSTALL = \
  ocamldep ocamlprof ocamlcp ocamloptp ocamlmktop ocamlmklib ocamlobjinfo

CAMLC = $(BOOT_OCAMLC) $(BOOT_STDLIBFLAGS) \
        -use-prims $(ROOTDIR)/runtime/primitives -I $(ROOTDIR)
CAMLOPT = $(OCAMLRUN) $(ROOTDIR)/ocamlopt$(EXE) $(STDLIBFLAGS)
VPATH = $(addprefix $(ROOTDIR)/,utils parsing typing bytecomp \
                   middle_end middle_end/closure middle_end/flambda \
                   middle_end/flambda/base_types driver toplevel \
                   file_formats lambda)
INCLUDES = $(addprefix -I ,$(VPATH))

.PHONY: all allopt opt.opt othertools # allopt and opt.opt are synonyms
all: $(TOOLS_BYTECODE_TARGETS)
opt.opt: $(TOOLS_NATIVE_TARGETS)
allopt: opt.opt

$(foreach TOOL, $(TOOLS_BYTECODE_TARGETS),\
  $(eval $(call OCAML_PROGRAM,$(TOOL))))

clean::
	rm -f $(TOOLS_BYTECODE_TARGETS) $(TOOLS_BYTECODE_TARGETS:=.exe)
	rm -f $(TOOLS_NATIVE_TARGETS) $(TOOLS_NATIVE_TARGETS:=.exe)

# The dependency generator

ocamldep_LIBRARIES = $(addprefix $(ROOTDIR)/compilerlibs/ocaml,common bytecomp)
ocamldep_MODULES = ocamldep

ocamldep$(EXE): OC_BYTECODE_LDFLAGS += -compat-32

# The profiler

ocamlprof_LIBRARIES =
ocamlprof_MODULES = \
  config build_path_prefix_map misc identifiable numbers arg_helper \
  local_store load_path clflags terminfo warnings location longident \
  docstrings syntaxerr ast_helper camlinternalMenhirLib parser pprintast \
  lexer parse ocamlprof

all: profiling.cmo
opt.opt: profiling.cmx

ocamlcp_ocamloptp_MODULES = \
  config build_path_prefix_map misc profile warnings identifiable numbers \
  arg_helper local_store load_path clflags terminfo location ccomp compenv \
  main_args ocamlcp_common

ocamlcp_LIBRARIES =
ocamlcp_MODULES = $(ocamlcp_ocamloptp_MODULES) ocamlcp

ocamloptp_LIBRARIES =
ocamloptp_MODULES = $(ocamlcp_ocamloptp_MODULES) ocamloptp

opt:: profiling.cmx

INSTALL_LIBDIR_PROFILING = $(INSTALL_LIBDIR)/profiling

install::
# If installing over a previous OCaml version, ensure the module is removed
# from the previous installation.
	rm -f "$(INSTALL_LIBDIR)"/profiling.cm* "$(INSTALL_LIBDIR)/profiling.$(O)"
	$(MKDIR) "$(INSTALL_LIBDIR_PROFILING)"
	$(INSTALL_DATA) \
	  profiling.cmi profiling.cmo \
	  "$(INSTALL_LIBDIR_PROFILING)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	  profiling.cmt profiling.cmti \
	  "$(INSTALL_LIBDIR_PROFILING)"
endif

installopt::
	$(INSTALL_DATA) \
          profiling.cmx profiling.$(O) \
	  "$(INSTALL_LIBDIR_PROFILING)"

# To help building mixed-mode libraries (OCaml + C)
ocamlmklib_LIBRARIES =
ocamlmklib_MODULES = config build_path_prefix_map misc ocamlmklib

# To make custom toplevels

ocamlmktop_LIBRARIES =
ocamlmktop_MODULES = \
  config build_path_prefix_map misc identifiable numbers arg_helper \
  local_store load_path clflags profile ccomp ocamlmktop

install::
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	for i in $(TOOLS_TO_INSTALL); \
	do \
	  $(INSTALL_PROG) "$$i$(EXE)" "$(INSTALL_BINDIR)/$$i.byte$(EXE)"; \
	  if test -f "$$i".opt$(EXE); then \
	    $(INSTALL_PROG) "$$i.opt$(EXE)" "$(INSTALL_BINDIR)" && \
	    (cd "$(INSTALL_BINDIR)" && $(LN) "$$i.opt$(EXE)" "$$i$(EXE)"); \
	  else \
	    (cd "$(INSTALL_BINDIR)" && $(LN) "$$i.byte$(EXE)" "$$i$(EXE)"); \
	  fi; \
	done
else
	for i in $(TOOLS_TO_INSTALL); \
	do \
	  if test -f "$$i".opt$(EXE); then \
	    $(INSTALL_PROG) "$$i.opt$(EXE)" "$(INSTALL_BINDIR)"; \
	    (cd "$(INSTALL_BINDIR)" && $(LN) "$$i.opt$(EXE)" "$$i$(EXE)"); \
	  fi; \
	done
endif

# The preprocessor for asm generators

cvt_emit = cvt_emit$(EXE)

$(eval $(call OCAML_BYTECODE_PROGRAM,cvt_emit))

cvt_emit_LIBRARIES =
cvt_emit_MODULES = cvt_emit

clean::
	rm -f cvt_emit.ml cvt_emit cvt_emit.exe

beforedepend:: cvt_emit.ml

# Reading cmt files

ocamlcmt_LIBRARIES = $(addprefix $(ROOTDIR)/compilerlibs/ocaml,common bytecomp)
ocamlcmt_MODULES = ocamlcmt

install::
	if test -f ocamlcmt.opt$(EXE); then \
	  $(INSTALL_PROG)\
	    ocamlcmt.opt$(EXE) "$(INSTALL_BINDIR)/ocamlcmt$(EXE)"; \
	else \
	  $(INSTALL_PROG) ocamlcmt$(EXE) "$(INSTALL_BINDIR)"; \
	fi

# The bytecode disassembler

dumpobj_LIBRARIES = $(addprefix $(ROOTDIR)/compilerlibs/ocaml,common bytecomp)
dumpobj_MODULES = opnames dumpobj

make_opcodes = make_opcodes$(EXE)

make_opcodes_LIBRARIES =
make_opcodes_MODULES = make_opcodes

$(eval $(call OCAML_BYTECODE_PROGRAM,make_opcodes))

opnames.ml: $(ROOTDIR)/runtime/caml/instruct.h $(make_opcodes)
	$(NEW_OCAMLRUN) $(make_opcodes) -opnames < $< > $@

clean::
	rm -f opnames.ml make_opcodes make_opcodes.exe make_opcodes.ml

beforedepend:: opnames.ml make_opcodes.ml

# Display info on compiled files

ocamlobjinfo_LIBRARIES = \
  $(addprefix $(ROOTDIR)/compilerlibs/ocaml,common bytecomp middleend)
ocamlobjinfo_MODULES = objinfo

# Scan object files for required primitives

primreq_LIBRARIES = $(addprefix $(ROOTDIR)/compilerlibs/ocaml,common bytecomp)
primreq_MODULES = primreq

lintapidiff_LIBRARIES = \
  $(addprefix $(ROOTDIR)/compilerlibs/ocaml,common bytecomp middleend) \
  $(ROOTDIR)/otherlibs/str/str
lintapidiff_MODULES = lintapidiff

lintapidiff.opt$(EXE): VPATH += $(ROOTDIR)/otherlibs/str

$(eval $(call OCAML_NATIVE_PROGRAM,lintapidiff))

clean::
	rm -f -- lintapidiff.opt lintapidiff.opt.exe
	rm -f lintapidiff.cm? lintapidiff.o lintapidiff.obj

# Copy a bytecode executable, stripping debug info

stripdebug_LIBRARIES = \
  $(addprefix $(ROOTDIR)/compilerlibs/ocaml,common bytecomp)
stripdebug_MODULES = stripdebug

# Compare two bytecode executables

cmpbyt_LIBRARIES = $(addprefix $(ROOTDIR)/compilerlibs/ocaml,common bytecomp)
cmpbyt_MODULES = cmpbyt

# Scan latex files, and run ocaml code examples

ocamltex = ocamltex$(EXE)

ocamltex_LIBRARIES = \
  $(addprefix $(ROOTDIR)/compilerlibs/ocaml,common bytecomp toplevel) \
  $(addprefix $(ROOTDIR)/otherlibs/,str/str unix/unix)
ocamltex_MODULES = ocamltex

# ocamltex uses str.cma and unix.cma and so must be compiled with
# $(ROOTDIR)/ocamlc not $(ROOTDIR)/boot/ocamlc since the boot
# compiler does not necessarily have the correct shared library
# configuration.
$(ocamltex): CAMLC = $(OCAMLRUN) $(ROOTDIR)/ocamlc$(EXE) $(STDLIBFLAGS)
$(ocamltex): OC_COMMON_LDFLAGS += -linkall
$(ocamltex): VPATH += $(addprefix $(ROOTDIR)/otherlibs/,str unix)

$(eval $(call OCAML_BYTECODE_PROGRAM,ocamltex))

ocamltex.cmo: OC_COMMON_CFLAGS += -no-alias-deps

# we need str and unix which depend on the bytecode version of other tools
# thus we use the othertools target
ifeq "$(build_ocamltex)" "true"
othertools: $(ocamltex)
endif
clean::
	rm -f -- ocamltex ocamltex.exe ocamltex.cm?

# Common stuff

%.cmo: %.ml
	$(CAMLC) $(OC_COMMON_CFLAGS) $(INCLUDES) -c $<

%.cmi: %.mli
	$(CAMLC) $(OC_COMMON_CFLAGS) $(INCLUDES) -c $<

%.cmx: %.ml
	$(CAMLOPT) $(OC_COMMON_CFLAGS) $(INCLUDES) $(OC_NATIVE_CFLAGS) -c $<

clean::
	rm -f *.cmo *.cmi *.cma *.dll *.so *.lib *.a

OC_OCAMLDEPDIRS = $(VPATH)
depend: beforedepend
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

.PHONY: clean distclean install beforedepend depend

distclean: clean
	rm -f eventlog_metadata *.bak

include .depend
