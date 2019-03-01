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

DYNLINKDIR=$(ROOTDIR)/otherlibs/dynlink
UNIXDIR=$(ROOTDIR)/otherlibs/$(UNIXLIB)
CAMLRUN ?= $(ROOTDIR)/boot/ocamlrun
CAMLYACC ?= $(ROOTDIR)/yacc/ocamlyacc$(EXE)

CAMLC=$(CAMLRUN) $(ROOTDIR)/ocamlc -g -nostdlib -I $(ROOTDIR)/stdlib
COMPFLAGS=$(INCLUDES) -absname -w +a-4-9-41-42-44-45-48 -warn-error A \
          -safe-string -strict-sequence -strict-formats
LINKFLAGS=-linkall -I $(UNIXDIR) -I $(DYNLINKDIR)
YACCFLAGS=
CAMLLEX=$(CAMLRUN) $(ROOTDIR)/boot/ocamllex
CAMLDEP=$(CAMLRUN) $(ROOTDIR)/boot/ocamlc -depend
DEPFLAGS=-slash
DEPINCLUDES=$(INCLUDES)

DIRECTORIES=$(UNIXDIR) $(DYNLINKDIR) $(addprefix $(ROOTDIR)/,\
  utils parsing typing bytecomp toplevel)

INCLUDES=$(addprefix -I ,$(DIRECTORIES))

utils_modules := $(addprefix utils/,\
  config build_path_prefix_map misc identifiable numbers arg_helper clflags \
  consistbl warnings terminfo load_path)

parsing_modules := $(addprefix parsing/,\
  location longident docstrings syntaxerr ast_helper ast_mapper ast_iterator \
  attr_helper builtin_attributes pprintast)

typing_modules := $(addprefix typing/,\
  ident path types btype primitive typedtree subst predef datarepr \
  cmi_format persistent_env env oprint ctype printtyp mtype envaux)

bytecomp_modules := $(addprefix bytecomp/,\
  runtimedef bytesections dll meta symtable opcodes)

other_compiler_modules := toplevel/genprintval

compiler_modules := $(addprefix $(ROOTDIR)/,\
  $(utils_modules) $(parsing_modules) $(typing_modules) \
  $(bytecomp_modules) $(other_compiler_modules))

debugger_modules := \
  int64ops primitives unix_tools debugger_config parameters lexer \
  input_handling question debugcom exec source pos checkpoints events \
  program_loading symbols breakpoints trap_barrier history printval \
  show_source time_travel program_management frames eval \
  show_information loadprinter parser command_line main

all_modules := $(compiler_modules) $(debugger_modules)

all_objects := $(addsuffix .cmo,$(all_modules))

all: ocamldebug$(EXE)

ocamldebug$(EXE): $(UNIXDIR)/unix.cma $(DYNLINKDIR)/dynlink.cma $(all_objects)
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
	$(CAMLDEP) $(DEPFLAGS) $(DEPINCLUDES) *.mli *.ml \
	| sed -e 's,$(UNIXDIR)/,$$(UNIXDIR)/,' > .depend

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll
clean::
	rm -f lexer.ml
beforedepend:: lexer.ml

parser.ml parser.mli: parser.mly
	$(CAMLYACC) parser.mly
clean::
	rm -f parser.ml parser.mli
beforedepend:: parser.ml parser.mli

include .depend
