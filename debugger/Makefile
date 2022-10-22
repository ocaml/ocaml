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

include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

DYNLINKDIR=$(ROOTDIR)/otherlibs/dynlink
UNIXDIR=$(ROOTDIR)/otherlibs/unix

CAMLC=$(BEST_OCAMLC) $(STDLIBFLAGS) -g
COMPFLAGS=$(INCLUDES) -absname -w +a-4-9-41-42-44-45-48-70 -warn-error +A \
          -strict-sequence -strict-formats
LINKFLAGS=-linkall -I $(UNIXDIR) -I $(DYNLINKDIR)
OC_OCAMLDEPDIRS = $(DIRECTORIES)

DIRECTORIES=$(UNIXDIR) $(DYNLINKDIR) $(addprefix $(ROOTDIR)/,\
  utils parsing typing bytecomp toplevel driver file_formats lambda)

INCLUDES=$(addprefix -I ,$(DIRECTORIES))

compiler_modules := $(ROOTDIR)/toplevel/genprintval

debugger_modules := \
  int64ops primitives unix_tools debugger_config parameters debugger_lexer \
  input_handling question debugcom exec source pos checkpoints events \
  program_loading symbols breakpoints trap_barrier history printval \
  show_source time_travel program_management frames eval \
  show_information loadprinter debugger_parser command_line main

compiler_objects := $(addsuffix .cmo,$(compiler_modules))

debugger_objects := $(addsuffix .cmo,$(debugger_modules))

libraries = $(ROOTDIR)/compilerlibs/ocamlcommon.cma \
  $(UNIXDIR)/unix.cma $(DYNLINKDIR)/dynlink.cma

all: ocamldebug$(EXE)

ocamldebug.cmo: $(debugger_objects)
	$(CAMLC) -pack $(COMPFLAGS) -o $@ $^

ocamldebug$(EXE): $(libraries) $(compiler_objects) ocamldebug.cmo \
    ocamldebug_entry.cmo
	$(CAMLC) $(LINKFLAGS) -o $@ -linkall $^

install:
	$(INSTALL_PROG) ocamldebug$(EXE) "$(INSTALL_BINDIR)"

.PHONY: clean
clean::
	rm -f ocamldebug ocamldebug.exe
	rm -f *.cmo *.cmi

.PHONY: distclean
distclean: clean

ocamldebug_entry.cmo: ocamldebug_entry.ml ocamldebug.cmo
	$(CAMLC) -c $(COMPFLAGS) $<

%.cmo: %.ml
	$(CAMLC) -c $(COMPFLAGS) -for-pack ocamldebug $<

%.cmi: %.mli
	$(CAMLC) -c $(COMPFLAGS) -for-pack ocamldebug $<

depend: beforedepend
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

clean::
	rm -f debugger_lexer.ml
beforedepend:: debugger_lexer.ml

clean::
	rm -f $(addprefix debugger_parser.,ml mli output)
beforedepend:: debugger_parser.ml debugger_parser.mli

include .depend
