#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Sebastien Hinderer, projet Gallium, INRIA Paris             *
#*                                                                        *
#*   Copyright 2016 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# The Makefile for ocamltest

ROOTDIR = ..

include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

ifeq "$(filter str,$(OTHERLIBRARIES))" ""
  str := false
else
  str := true
endif

ifeq "$(filter systhreads,$(OTHERLIBRARIES))" ""
  systhreads := false
else
  systhreads := true
endif

ifeq "$(filter unix,$(OTHERLIBRARIES))" ""
  ocamltest_unix := dummy
  unix_name :=
  unix_path :=
  unix := None
  unix_include :=
else
  ocamltest_unix := real
  unix_name := unix
  unix_path := $(ROOTDIR)/otherlibs/unix
  unix_include := -I $(unix_path) $(EMPTY)
  ifeq "$(UNIX_OR_WIN32)" "win32"
    unix := Some false
  else
    unix := Some true
  endif
endif

ifeq "$(UNIX_OR_WIN32)" "win32"
  ocamlsrcdir := $(shell echo "$(abspath $(shell pwd)/..)" | cygpath -w -f -)
  CSC := csc
  ifeq "$(HOST:i686-%=i686)" "i686"
    CSCFLAGS := /platform:x86
  else
    CSCFLAGS :=
  endif
  CSCFLAGS += /nologo /nowarn:1668
else
  ocamlsrcdir := $(abspath $(shell pwd)/..)
  CSC :=
  CSCFLAGS :=
endif

ifeq "$(TOOLCHAIN)" "msvc"
CPP := $(CPP) 2> nul
endif

ifeq "$(WITH_OCAMLDOC)" "ocamldoc"
WITH_OCAMLDOC := true
else
WITH_OCAMLDOC := false
endif

ifeq "$(WITH_DEBUGGER)" "ocamldebugger"
WITH_OCAMLDEBUG := true
else
WITH_OCAMLDEBUG := false
endif

ifdef UNIX_OR_WIN32
run_source := run_$(UNIX_OR_WIN32).c
else
ifneq "$(filter-out $(CLEAN_TARGET_NAMES), $(MAKECMDGOALS))" ""
$(warning The variable UNIX_OR_WIN32 is not defined. \
  It must be set (usually by $(ROOTDIR)/configure), \
  or only clean rules are supported.)
endif
# If we are in a 'clean' rule, we ask for both versions to be cleaned.
run_source := run_unix.c run_win32.c
endif

# List of source files from which ocamltest is compiled
# (all the different sorts of files are derived from this)

# ocamltest has two components: its core and the OCaml "plugin"
# which is actually built into the tool but clearly separated from its core

core := \
  $(run_source) run_stubs.c \
  ocamltest_config.mli ocamltest_config.ml.in \
  ocamltest_unix.mli ocamltest_unix.ml \
  ocamltest_stdlib.mli ocamltest_stdlib.ml \
  run_command.mli run_command.ml \
  filecompare.mli filecompare.ml \
  variables.mli variables.ml \
  environments.mli environments.ml \
  result.mli result.ml \
  actions.mli actions.ml \
  tests.mli tests.ml \
  strace.mli strace.ml \
  tsl_ast.mli tsl_ast.ml \
  tsl_parser.mly \
  tsl_lexer.mli tsl_lexer.mll \
  modifier_parser.mli modifier_parser.ml \
  tsl_semantics.mli tsl_semantics.ml \
  builtin_variables.mli builtin_variables.ml \
  actions_helpers.mli actions_helpers.ml \
  builtin_actions.mli builtin_actions.ml

ocaml_plugin := \
  ocaml_backends.mli ocaml_backends.ml \
  ocaml_filetypes.mli ocaml_filetypes.ml \
  ocaml_variables.mli ocaml_variables.ml \
  ocaml_modifiers.mli ocaml_modifiers.ml \
  ocaml_directories.mli ocaml_directories.ml \
  ocaml_files.mli ocaml_files.ml \
  ocaml_flags.mli ocaml_flags.ml \
  ocaml_commands.mli ocaml_commands.ml \
  ocaml_tools.mli ocaml_tools.ml \
  ocaml_compilers.mli ocaml_compilers.ml \
  ocaml_toplevels.mli ocaml_toplevels.ml \
  ocaml_actions.mli ocaml_actions.ml \
  ocaml_tests.mli ocaml_tests.ml

sources := $(core) $(ocaml_plugin) \
  options.mli options.ml \
  main.mli main.ml

# List of .ml files used for ocamldep and to get the list of modules

ml_files := \
  $(filter %.ml, \
    $(subst .ml.in,.ml,$(subst .mll,.ml,$(subst .mly,.ml,$(sources)))) \
  )

cmo_files := $(ml_files:.ml=.cmo)

cmx_files := $(ml_files:.ml=.cmx)

# List of .mli files for ocamldep
mli_files := \
  $(filter %.mli,$(subst .mly,.mli,$(sources)))

cmi_files := $(mli_files:.mli=.cmi)

c_files := $(filter %.c, $(sources))

o_files := $(c_files:.c=.$(O))

lexers := $(filter %.mll,$(sources))

parsers := $(filter %.mly,$(sources))

config_files := $(filter %.ml.in,$(sources))

dependencies_generated_prereqs := \
  ocamltest_unix.ml \
  $(config_files:.ml.in=.ml) \
  $(lexers:.mll=.ml) \
  $(parsers:.mly=.mli) $(parsers:.mly=.ml)

generated := $(dependencies_generated_prereqs) $(parsers:.mly=.output)

bytecode_modules := $(o_files) $(cmo_files)

native_modules := $(o_files) $(cmx_files)

directories := $(addprefix $(ROOTDIR)/,utils bytecomp parsing \
                                       compilerlibs file_formats)

include_directories := $(addprefix -I , $(directories))

flags := $(STDLIBFLAGS) -g $(include_directories) \
  -strict-sequence -safe-string -strict-formats \
  -w +a-4-9-41-42-44-45-48 -warn-error +A-70

ocamlc = $(BEST_OCAMLC) $(flags)

ocamlopt = $(BEST_OCAMLOPT) $(flags)

.SECONDARY: $(lexers:.mll=.ml) $(parsers:.mly=.mli) $(parsers:.mly=.ml)

.PHONY: all allopt opt.opt # allopt and opt.opt are synonyms
all: ocamltest$(EXE)
allopt: ocamltest.opt$(EXE)
opt.opt: allopt

compdeps_names=ocamlcommon ocamlbytecomp
compdeps_paths=$(addprefix $(ROOTDIR)/compilerlibs/,$(compdeps_names))
deps_paths=$(compdeps_paths) $(addprefix $(unix_path)/,$(unix_name))
deps_byte=$(addsuffix .cma,$(deps_paths))
deps_opt=$(addsuffix .cmxa,$(deps_paths))

$(eval $(call PROGRAM_SYNONYM,ocamltest))

ocamltest_unix.%: flags+=$(unix_include) -opaque

ocamltest$(EXE): $(deps_byte) $(bytecode_modules)
	$(ocamlc_cmd) $(unix_include)-custom -o $@ $^

%.cmo: %.ml $(deps_byte)
	$(ocamlc) -c $<

$(eval $(call PROGRAM_SYNONYM,ocamltest.opt))

ocamltest.opt$(EXE): $(deps_opt) $(native_modules)
	$(ocamlopt_cmd) $(unix_include)-o $@ $^

%.cmx: %.ml $(deps_opt)
	$(ocamlopt) -c $<

%.cmi: %.mli $(deps_byte)
	$(ocamlc) -c $<

ocamltest_unix.ml: ocamltest_unix_$(ocamltest_unix).ml
	echo '# 1 "$^"' > $@
	cat $^ >> $@

ocamltest_config.ml: ocamltest_config.ml.in Makefile ../Makefile.config
	sed $(call SUBST,AFL_INSTRUMENT) \
	    $(call SUBST,INSTRUMENTED_RUNTIME) \
	    $(call SUBST,ARCH) \
	    $(call SUBST,SUPPORTS_SHARED_LIBRARIES) \
	    $(call SUBST,unix) \
	    $(call SUBST,systhreads) \
	    $(call SUBST,str) \
	    $(call SUBST,SYSTEM) \
	    $(call SUBST_STRING,CPP) \
	    $(call SUBST_STRING,ocamlsrcdir) \
	    $(call SUBST,FLAMBDA) \
	    $(call SUBST,FLAT_FLOAT_ARRAY) \
	    $(call SUBST,WITH_OCAMLDOC) \
	    $(call SUBST,WITH_OCAMLDEBUG) \
	    $(call SUBST,O) \
	    $(call SUBST,A) \
	    $(call SUBST,S) \
	    $(call SUBST,NATIVE_COMPILER) \
	    $(call SUBST,NATDYNLINK) \
	    $(call SUBST_STRING,SHAREDLIB_CFLAGS) \
	    $(call SUBST,SO) \
	    $(call SUBST_STRING,CSC) \
	    $(call SUBST_STRING,CSCFLAGS) \
	    $(call SUBST_STRING,EXE) \
	    $(call SUBST_STRING,MKDLL_EXP) \
	    $(call SUBST_STRING,MKEXE_EXP) \
	    $(call SUBST_STRING,BYTECCLIBS) \
	    $(call SUBST_STRING,NATIVECCLIBS) \
	    $(call SUBST_STRING,ASM) \
	    $(call SUBST_STRING,CC) \
	    $(call SUBST_STRING,OC_CFLAGS) \
	    $(call SUBST,CCOMPTYPE) \
	    $(call SUBST,DIFF) \
	    $(call SUBST,DIFF_FLAGS) \
	    $(call SUBST,WINDOWS_UNICODE) \
	    $(call SUBST,FUNCTION_SECTIONS) \
	    $< > $@

# Manual

.PHONY: doc

doc: ocamltest.html

ocamltest.html: ocamltest.org
	pandoc -s --toc -N -f org -t html -o $@ $<

.PHONY: clean
clean:
	rm -rf ocamltest ocamltest.exe ocamltest.opt ocamltest.opt.exe
	rm -rf $(c_files:.c=.o) $(c_files:.c=.obj)
	rm -rf $(ml_files:.ml=.o) $(ml_files:.ml=.obj)
	rm -rf $(cmi_files)
	rm -rf $(cmo_files)
	rm -rf $(cmx_files)
	rm -rf $(generated)
	rm -f ocamltest.html
	rm -rf $(DEPDIR)

ifeq "$(COMPUTE_DEPS)" "true"
include $(addprefix $(DEPDIR)/, $(c_files:.c=.$(D)))
endif

.PHONY: distclean
distclean: clean

$(DEPDIR)/%.$(D): %.c | $(DEPDIR)
	$(DEP_CC) $(OC_CPPFLAGS) $(CPPFLAGS) $< -MT '$*.$(O)' -MF $@

.PHONY: depend
depend: $(dependencies_generated_prereqs)
	$(OCAMLDEP_CMD) $(mli_files) $(ml_files) > .depend

-include .depend
