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

-include $(ROOTDIR)/Makefile.config
-include $(ROOTDIR)/Makefile.common
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

ifeq "$(filter $(UNIXLIB),$(OTHERLIBRARIES))" ""
  unix := None
else
  ifeq "$(UNIX_OR_WIN32)" "win32"
    unix := Some false
  else
    unix := Some true
  endif
endif

ifeq "$(UNIX_OR_WIN32)" "win32"
  ocamlsrcdir := $(shell echo "$(abspath $(shell pwd)/..)"|cygpath -w -f - \
    | sed 's/\\/\\\\\\\\/g')
  mkexe := $(MKEXE_ANSI) -link $(OC_LDFLAGS)
else
  ocamlsrcdir := $(abspath $(shell pwd)/..)
  mkexe := $(MKEXE)
endif

ifeq "$(TOOLCHAIN)" "msvc"
CPP := $(CPP) 2> nul
CSC := csc
ifeq "$(HOST)" "i686-pc-windows"
CSCFLAGS := /platform:x86
else
CSCFLAGS :=
endif
CSCFLAGS += /nologo /nowarn:1668
else
CSC :=
CSCFLAGS :=
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

OC_CPPFLAGS += -I$(ROOTDIR)/runtime -DCAML_INTERNALS

run := run_$(UNIX_OR_WIN32)

# List of source files from which ocamltest is compiled
# (all the different sorts of files are derived from this)

# ocamltest has two components: its core and the OCaml "plugin"
# which is actually built into the tool but clearly separated from its core

core := \
  $(run).c \
  run_stubs.c \
  ocamltest_stdlib_stubs.c \
  ocamltest_config.mli ocamltest_config.ml.in \
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

ocaml_objects := $(ml_files:.ml=.$(O))

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
  $(config_files:.ml.in=.ml) \
  $(lexers:.mll=.ml) \
  $(parsers:.mly=.mli) $(parsers:.mly=.ml)

generated := $(dependencies_generated_prereqs) $(parsers:.mly=.output)

bytecode_modules := $(o_files) $(cmo_files)

native_modules := $(o_files) $(cmx_files)

directories := $(addprefix $(ROOTDIR)/,utils bytecomp parsing stdlib \
                                       compilerlibs file_formats)

include_directories := $(addprefix -I , $(directories))

flags := -g -nostdlib $(include_directories) \
  -strict-sequence -safe-string -strict-formats \
  -w +a-4-9-41-42-44-45-48 -warn-error A

ocamlc := $(BEST_OCAMLC) $(flags)

ocamlopt :=  $(BEST_OCAMLOPT) $(flags)

ocamldep := $(BEST_OCAMLDEP)
depflags := -slash
depincludes :=

ocamllex := $(BEST_OCAMLLEX)

ocamlyacc := $(ROOTDIR)/yacc/ocamlyacc

ocamlcdefaultflags :=

ocamloptdefaultflags := $(shell ./getocamloptdefaultflags $(TARGET))

.SECONDARY: $(lexers:.mll=.ml) $(parsers:.mly=.mli) $(parsers:.mly=.ml)

.PHONY: all allopt opt.opt # allopt and opt.opt are synonyms
all: ocamltest$(EXE)
allopt: ocamltest.opt$(EXE)
opt.opt: allopt

compdeps_names=ocamlcommon ocamlbytecomp
compdeps_paths=$(addprefix $(ROOTDIR)/compilerlibs/,$(compdeps_names))
compdeps_byte=$(addsuffix .cma,$(compdeps_paths))
compdeps_opt=$(addsuffix .cmxa,$(compdeps_paths))

ocamltest$(EXE): $(compdeps_byte) $(bytecode_modules)
	$(ocamlc_cmd) -custom -o $@ $^

%.cmo: %.ml $(compdeps_byte)
	$(ocamlc) -c $<

ocamltest.opt$(EXE): $(compdeps_opt) $(native_modules)
	$(ocamlopt_cmd) -o $@ $^

%.cmx: %.ml $(compdeps_opt)
	$(ocamlopt) -c $<

%.cmi: %.mli $(compdeps_byte)
	$(ocamlc) -c $<

%.ml %.mli: %.mly
	$(ocamlyacc) $<

%.ml: %.mll
	$(ocamllex) $(OCAMLLEX_FLAGS) $<

ocamltest_config.ml: ocamltest_config.ml.in Makefile ../Makefile.config
	sed \
	  -e 's|@@AFL_INSTRUMENT@@|$(AFL_INSTRUMENT)|' \
	  -e 's|@@RUNTIMEI@@|$(RUNTIMEI)|' \
	  -e 's|@@ARCH@@|$(ARCH)|' \
	  -e 's|@@SHARED_LIBRARIES@@|$(SUPPORTS_SHARED_LIBRARIES)|' \
	  -e 's|@@UNIX@@|$(unix)|' \
	  -e 's|@@SYSTHREADS@@|$(systhreads)|' \
	  -e 's|@@STR@@|$(str)|' \
	  -e 's|@@SYSTEM@@|$(SYSTEM)|' \
	  -e 's|@@CPP@@|$(CPP)|' \
	  -e 's|@@OCAMLCDEFAULTFLAGS@@|$(ocamlcdefaultflags)|' \
	  -e 's|@@OCAMLOPTDEFAULTFLAGS@@|$(ocamloptdefaultflags)|' \
	  -e 's|@@OCAMLSRCDIR@@|$(ocamlsrcdir)|' \
	  -e 's|@@FLAMBDA@@|$(FLAMBDA)|' \
	  -e 's|@@SPACETIME@@|$(WITH_SPACETIME)|' \
	  -e 's|@@FORCE_SAFE_STRING@@|$(FORCE_SAFE_STRING)|' \
	  -e 's|@@FLAT_FLOAT_ARRAY@@|$(FLAT_FLOAT_ARRAY)|' \
	  -e 's|@@OCAMLDOC@@|$(WITH_OCAMLDOC)|' \
	  -e 's|@@OCAMLDEBUG@@|$(WITH_OCAMLDEBUG)|' \
	  -e 's|@@OBJEXT@@|$(O)|' \
	  -e 's|@@ASMEXT@@|$(S)|' \
	  -e 's|@@NATIVE_DYNLINK@@|$(NATDYNLINK)|' \
	  -e 's|@@SHARED_LIBRARY_CFLAGS@@|$(SHAREDLIB_CFLAGS)|' \
	  -e 's|@@SHAREDOBJEXT@@|$(SO)|' \
	  -e 's|@@CSC@@|$(CSC)|' \
	  -e 's|@@CSCFLAGS@@|$(CSCFLAGS)|' \
	  -e 's|@@MKDLL@@|$(MKDLL)|' \
	  -e 's|@@MKEXE@@|$(mkexe)|' \
	  -e 's|@@BYTECCLIBS@@|$(BYTECCLIBS)|' \
	  -e 's|@@NATIVECCLIBS@@|$(NATIVECCLIBS)|' \
	  -e 's|@@ASM@@|$(ASM)|' \
	  -e 's|@@CC@@|$(CC)|' \
	  -e 's|@@CFLAGS@@|$(OC_CFLAGS)|' \
	  -e 's|@@CCOMPTYPE@@|$(CCOMPTYPE)|' \
	  -e 's|@@WINDOWS_UNICODE@@|$(WINDOWS_UNICODE)|' \
	  -e 's|@@FUNCTION_SECTIONS@@|$(FUNCTION_SECTIONS)|' \
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
	rm -rf run_unix.o run_win32.o run_win32.obj
	rm -rf $(ml_files:.ml=.o) $(ml_files:.ml=.obj)
	rm -rf $(cmi_files)
	rm -rf $(cmo_files)
	rm -rf $(cmx_files)
	rm -rf $(generated)
	rm -f ocamltest.html

ifneq "$(TOOLCHAIN)" "msvc"
.PHONY: depend
depend: $(dependencies_generated_prereqs)
	$(CC) -MM $(OC_CPPFLAGS) $(c_files) \
	  | sed -e 's/\.o/.$$(O)/' > .depend
	$(ocamldep) $(depflags) $(depincludes) $(mli_files) $(ml_files) \
	  >> .depend
endif

-include .depend
