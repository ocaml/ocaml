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

# Setup GNU make variables storing per-target source and target,
# a list of installed tools, and a function to quote a filename for
# the shell.
installed_tools := ocamldep ocamlprof ocamlcp ocamloptp \
                   ocamlmktop ocamlmklib ocamlobjinfo

install_files :=
define byte2native
$(patsubst %.cmo,%.cmx,$(patsubst %.cma,%.cmxa,$1))
endef

CAMLC = $(BOOT_OCAMLC) $(BOOT_STDLIBFLAGS) -g \
        -use-prims $(ROOTDIR)/runtime/primitives -I $(ROOTDIR)
CAMLOPT = $(OCAMLRUN) $(ROOTDIR)/ocamlopt$(EXE) $(STDLIBFLAGS) -g
DIRS = $(addprefix $(ROOTDIR)/,utils parsing typing bytecomp \
                   middle_end middle_end/closure middle_end/flambda \
                   middle_end/flambda/base_types driver toplevel \
                   file_formats lambda)
INCLUDES = $(addprefix -I ,$(DIRS))
COMPFLAGS = -absname -w +a-4-9-41-42-44-45-48-70 -strict-sequence \
-warn-error +A -principal -strict-formats -bin-annot $(INCLUDES)
LINKFLAGS = $(INCLUDES)
VPATH := $(filter-out -I,$(INCLUDES))

programs_byte := \
  ocamldep ocamlprof ocamlcp ocamloptp ocamlmklib  \
  ocamlmktop ocamlcmt dumpobj ocamlobjinfo \
  primreq stripdebug cmpbyt
install_files += $(filter $(installed_tools), $(programs_byte))
programs_opt := $(programs_byte:%=%.opt)

.PHONY: all allopt opt.opt othertools # allopt and opt.opt are synonyms
all: $(programs_byte)
opt.opt: $(programs_opt)
allopt: opt.opt

$(foreach program, $(programs_byte) $(programs_opt),\
  $(eval $(call PROGRAM_SYNONYM,$(program))))

$(programs_byte:%=%$(EXE)):
	$(CAMLC) $(LINKFLAGS) -I $(ROOTDIR) -o $@ $(filter-out %.cmi,$^)

$(programs_opt:%=%$(EXE)):
	$(CAMLOPT_CMD) $(LINKFLAGS) -I $(ROOTDIR) -o $@ $(filter-out %.cmi,$^)

clean::
	rm -f $(programs_byte) $(programs_byte:%=%.exe)
	rm -f $(programs_opt) $(programs_opt:%=%.exe)

# The dependency generator

ocamldep_objects = \
  $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
  $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
  ocamldep.cmo depend.cmi

ocamldep$(EXE): LINKFLAGS += -compat-32
ocamldep$(EXE): $(ocamldep_objects)
ocamldep.opt$(EXE): $(call byte2native, $(ocamldep_objects))

# The profiler

OCAMLPROF=config.cmo build_path_prefix_map.cmo misc.cmo identifiable.cmo \
  numbers.cmo arg_helper.cmo local_store.cmo load_path.cmo clflags.cmo \
  terminfo.cmo warnings.cmo location.cmo longident.cmo docstrings.cmo \
  syntaxerr.cmo ast_helper.cmo \
  camlinternalMenhirLib.cmo parser.cmo \
  pprintast.cmo \
  lexer.cmo parse.cmo ocamlprof.cmo

ocamlprof$(EXE): $(OCAMLPROF)
ocamlprof.opt$(EXE): $(call byte2native, $(OCAMLPROF))
all: profiling.cmo
opt.opt: profiling.cmx

OCAMLCP = config.cmo build_path_prefix_map.cmo misc.cmo profile.cmo \
          warnings.cmo identifiable.cmo numbers.cmo arg_helper.cmo \
          local_store.cmo load_path.cmo clflags.cmo \
          terminfo.cmo location.cmo ccomp.cmo compenv.cmo \
          main_args.cmo ocamlcp_common.cmo

ocamlcp$(EXE): $(OCAMLCP) ocamlcp.cmo
ocamlcp.opt$(EXE): $(call byte2native, $(OCAMLCP) ocamlcp.cmo)
ocamloptp$(EXE): $(OCAMLCP) ocamloptp.cmo
ocamloptp.opt$(EXE): $(call byte2native, $(OCAMLCP) ocamloptp.cmo)

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
OCAMLMKLIB = config.cmo build_path_prefix_map.cmo misc.cmo ocamlmklib.cmo

ocamlmklib$(EXE): $(OCAMLMKLIB)
ocamlmklib.opt$(EXE): $(call byte2native, $(OCAMLMKLIB))

# To make custom toplevels

OCAMLMKTOP=config.cmo build_path_prefix_map.cmo misc.cmo \
       identifiable.cmo numbers.cmo arg_helper.cmo local_store.cmo \
       load_path.cmo clflags.cmo profile.cmo ccomp.cmo ocamlmktop.cmo

ocamlmktop$(EXE): $(OCAMLMKTOP)
ocamlmktop.opt$(EXE): $(call byte2native, $(OCAMLMKTOP))
# othertools means "after the toplevel has been built" here
othertools: ocamlmktop_init.cmo
INSTALL_LIBDIR_OCAMLMKTOP = $(INSTALL_LIBDIR)/ocamlmktop
install::
	$(MKDIR) "$(INSTALL_LIBDIR_OCAMLMKTOP)"
	$(INSTALL_DATA) \
    ocamlmktop_init.cmi ocamlmktop_init.cmo \
    "$(INSTALL_LIBDIR_OCAMLMKTOP)"

# Converter olabl/ocaml 2.99 to ocaml 3

LIBRARY3=config.cmo build_path_prefix_map.cmo misc.cmo warnings.cmo location.cmo

ifeq ($(UNIX_OR_WIN32),unix)
LN := ln -sf
else
LN := cp -pf
endif

install::
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	for i in $(install_files); \
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
	for i in $(install_files); \
	do \
	  if test -f "$$i".opt$(EXE); then \
	    $(INSTALL_PROG) "$$i.opt$(EXE)" "$(INSTALL_BINDIR)"; \
	    (cd "$(INSTALL_BINDIR)" && $(LN) "$$i.opt$(EXE)" "$$i$(EXE)"); \
	  fi; \
	done
endif

# The preprocessor for asm generators

cvt_emit := cvt_emit$(EXE)

$(eval $(call PROGRAM_SYNONYM,cvt_emit))

$(cvt_emit): cvt_emit.cmo
	$(CAMLC) $(LINKFLAGS) -o $@ $^

clean::
	rm -f cvt_emit.ml cvt_emit cvt_emit.exe

beforedepend:: cvt_emit.ml

# Reading cmt files

OCAMLCMT = \
          $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
          $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
          ocamlcmt.cmo

ocamlcmt$(EXE): $(OCAMLCMT)
ocamlcmt.opt$(EXE): $(call byte2native, $(OCAMLCMT))

install::
	if test -f ocamlcmt.opt$(EXE); then \
	  $(INSTALL_PROG)\
	    ocamlcmt.opt$(EXE) "$(INSTALL_BINDIR)/ocamlcmt$(EXE)"; \
	else \
	  $(INSTALL_PROG) ocamlcmt$(EXE) "$(INSTALL_BINDIR)"; \
	fi

# The bytecode disassembler

DUMPOBJ= \
          $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
          $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
          opnames.cmo dumpobj.cmo

dumpobj$(EXE): $(DUMPOBJ)
dumpobj.opt$(EXE): $(call byte2native, $(DUMPOBJ))

make_opcodes := make_opcodes$(EXE)

$(eval $(call PROGRAM_SYNONYM,make_opcodes))

$(make_opcodes): make_opcodes.ml
	$(CAMLC) $< -o $@

opnames.ml: $(ROOTDIR)/runtime/caml/instruct.h $(make_opcodes)
	$(NEW_OCAMLRUN) $(make_opcodes) -opnames < $< > $@

clean::
	rm -f opnames.ml make_opcodes make_opcodes.exe make_opcodes.ml

beforedepend:: opnames.ml

# Display info on compiled files

DEF_SYMBOL_PREFIX = '-Dsymbol_prefix=""'

ifeq "$(SYSTEM)" "macosx"
DEF_SYMBOL_PREFIX = '-Dsymbol_prefix="_"'
endif

ifeq "$(SYSTEM)" "cygwin"
DEF_SYMBOL_PREFIX = '-Dsymbol_prefix="_"'
endif

OCAMLOBJINFO=$(ROOTDIR)/compilerlibs/ocamlcommon.cma \
             $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
             $(ROOTDIR)/compilerlibs/ocamlmiddleend.cma \
             objinfo.cmo

ocamlobjinfo$(EXE): $(OCAMLOBJINFO)
ocamlobjinfo.opt$(EXE): $(call byte2native, $(OCAMLOBJINFO))

PRIMREQ=$(ROOTDIR)/compilerlibs/ocamlcommon.cma \
        $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
        primreq.cmo

# Scan object files for required primitives
primreq$(EXE): $(PRIMREQ)
primreq.opt$(EXE): $(call byte2native, $(PRIMREQ))

LINTAPIDIFF=$(ROOTDIR)/compilerlibs/ocamlcommon.cmxa \
        $(ROOTDIR)/compilerlibs/ocamlbytecomp.cmxa \
        $(ROOTDIR)/compilerlibs/ocamlmiddleend.cmxa \
        $(ROOTDIR)/otherlibs/str/str.cmxa \
        lintapidiff.cmx

lintapidiff.opt$(EXE): INCLUDES+= -I $(ROOTDIR)/otherlibs/str
lintapidiff.opt$(EXE): $(LINTAPIDIFF)
	$(CAMLOPT_CMD) $(LINKFLAGS) -I $(ROOTDIR) -o $@ $(LINTAPIDIFF)
clean::
	rm -f -- lintapidiff.opt lintapidiff.opt.exe
	rm -f lintapidiff.cm? lintapidiff.o lintapidiff.obj

# Copy a bytecode executable, stripping debug info

STRIPDEBUG=$(ROOTDIR)/compilerlibs/ocamlcommon.cma \
           $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
           stripdebug.cmo

stripdebug$(EXE): $(STRIPDEBUG)
stripdebug.opt$(EXE): $(call byte2native, $(STRIPDEBUG))

# Compare two bytecode executables

CMPBYT=$(ROOTDIR)/compilerlibs/ocamlcommon.cma \
       $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
       cmpbyt.cmo

cmpbyt$(EXE): $(CMPBYT)
cmpbyt.opt$(EXE): $(call byte2native, $(CMPBYT))

caml_tex_files := \
  $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
  $(ROOTDIR)/compilerlibs/ocamlbytecomp.cma \
  $(ROOTDIR)/compilerlibs/ocamltoplevel.cma \
  $(ROOTDIR)/otherlibs/str/str.cma \
  $(ROOTDIR)/otherlibs/unix/unix.cma \
  caml_tex.ml

# checkstack tool

checkstack$(EXE): checkstack.$(O)
	$(MKEXE) $(OUTPUTEXE)$@ $<

#Scan latex files, and run ocaml code examples

caml_tex := caml-tex$(EXE)

# caml-tex uses str.cma and unix.cma and so must be compiled with
# $(ROOTDIR)/ocamlc not $(ROOTDIR)/boot/ocamlc since the boot
# compiler does not necessarily have the correct shared library
# configuration.
$(caml_tex): INCLUDES += $(addprefix -I $(ROOTDIR)/otherlibs/,str unix)
$(caml_tex): $(caml_tex_files)
	$(OCAMLRUN) $(ROOTDIR)/ocamlc$(EXE) $(STDLIBFLAGS) \
	  $(LINKFLAGS) -linkall -o $@ -no-alias-deps $^

# we need str and unix which depend on the bytecode version of other tools
# thus we use the othertools target
ifneq "$(WITH_CAMLTEX)" ""
othertools: $(caml_tex)
endif
clean::
	rm -f -- caml-tex caml-tex.exe caml_tex.cm?

# Common stuff

%.cmo: %.ml
	$(CAMLC) -c $(COMPFLAGS) - $<

%.cmi: %.mli
	$(CAMLC) -c $(COMPFLAGS) - $<

%.cmx: %.ml
	$(CAMLOPT) $(COMPFLAGS) -c - $<

clean::
	rm -f *.cmo *.cmi *.cma *.dll *.so *.lib *.a

OC_OCAMLDEPDIRS = $(DIRS)
depend: beforedepend
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

.PHONY: clean distclean install beforedepend depend

distclean: clean
	rm -f eventlog_metadata *.bak

include .depend
