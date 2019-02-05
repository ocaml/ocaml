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

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common

UNIXDIR=$(ROOTDIR)/otherlibs/$(UNIXLIB)
CAMLRUN ?= $(ROOTDIR)/boot/ocamlrun
CAMLYACC ?= $(ROOTDIR)/yacc/ocamlyacc$(EXE)

CAMLC=$(CAMLRUN) $(ROOTDIR)/ocamlc -g -nostdlib -I $(ROOTDIR)/stdlib
COMPFLAGS=$(INCLUDES) -absname -w +a-4-9-41-42-44-45-48 -warn-error A \
          -safe-string -strict-sequence -strict-formats -open Ocaml_common \
          -no-alias-deps
LINKFLAGS=-linkall -I $(UNIXDIR)
YACCFLAGS=
CAMLLEX=$(CAMLRUN) $(ROOTDIR)/boot/ocamllex
CAMLDEP=$(CAMLRUN) $(ROOTDIR)/boot/ocamlc -depend
DEPFLAGS=-slash
DEPINCLUDES=$(INCLUDES)

DIRECTORIES=$(UNIXDIR) $(ROOTDIR)/compilerlibs

INCLUDES=$(addprefix -I ,$(DIRECTORIES))

utils_modules := \
  config build_path_prefix_map misc identifiable numbers arg_helper clflags \
  consistbl warnings terminfo load_path

parsing_modules := \
  location longident docstrings syntaxerr ast_helper ast_mapper ast_iterator \
  attr_helper builtin_attributes pprintast

typing_modules := \
  ident path types btype primitive typedtree subst predef datarepr \
  cmi_format env oprint ctype printtyp mtype envaux

bytecomp_modules := \
  runtimedef bytesections dll meta symtable opcodes

other_compiler_modules := compdynlink_types \
  compdynlink_platform_intf \
  compdynlink_common compdynlink

compiler_modules := \
  $(utils_modules) $(parsing_modules) $(typing_modules) \
  $(bytecomp_modules) $(other_compiler_modules)

compiler_modules := \
  $(addprefix $(ROOTDIR)/compilerlibs/ocaml_common__,$(compiler_modules)) \
  $(ROOTDIR)/compilerlibs/ocaml_toplevel__genprintval

debugger_modules := \
  int64ops primitives unix_tools debugger_config parameters debug_lexer \
  input_handling question debugcom exec source pos checkpoints events \
  program_loading symbols breakpoints trap_barrier history printval \
  show_source time_travel program_management frames eval \
  show_information loadprinter debug_parser command_line main

all_modules := $(compiler_modules) $(debugger_modules)

all_objects := $(addsuffix .cmo,$(all_modules))

all: ocamldebug$(EXE)

ocamldebug$(EXE): $(UNIXDIR)/unix.cma $(all_objects)
	$(CAMLC) $(LINKFLAGS) -o $@ -linkall $^

install:
	$(INSTALL_PROG) ocamldebug$(EXE) "$(INSTALL_BINDIR)/ocamldebug$(EXE)"

clean::
	rm -f ocamldebug$(EXE)
	rm -f *.cmo *.cmi

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) $(DEPFLAGS) $(DEPINCLUDES) -open Ocaml_common *.mli *.ml \
	| sed -e 's,$(UNIXDIR)/,$$(UNIXDIR)/,' > .depend

debug_lexer.ml: debug_lexer.mll
	$(CAMLLEX) debug_lexer.mll
clean::
	rm -f debug_lexer.ml
beforedepend:: debug_lexer.ml

debug_parser.ml debug_parser.mli: debug_parser.mly
	$(CAMLYACC) debug_parser.mly
clean::
	rm -f debug_parser.ml debug_parser.mli
beforedepend:: debug_parser.ml debug_parser.mli

include .depend
