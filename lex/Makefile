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

# The lexer generator

ROOTDIR = ..

# NOTE: it is important that the OCAMLDEP and OCAMLLEX variables
# are defined *before* Makefile.common gets included, so that
# their local definitions here take precedence over their
# general shared definitions in Makefile.common.

OCAMLDEP ?= $(BOOT_OCAMLDEP)
OCAMLLEX ?= $(BOOT_OCAMLLEX)

include $(ROOTDIR)/Makefile.common

CAMLC = $(BOOT_OCAMLC) $(BOOT_STDLIBFLAGS) \
  -use-prims $(ROOTDIR)/runtime/primitives
CAMLOPT = $(OCAMLRUN) $(ROOTDIR)/ocamlopt$(EXE) $(STDLIBFLAGS)
COMPFLAGS = -g -absname -w +a-4-9-41-42-44-45-48 -warn-error +A \
            -safe-string -strict-sequence -strict-formats -bin-annot
LINKFLAGS = -g

OBJS=cset.cmo syntax.cmo parser.cmo lexer.cmo table.cmo lexgen.cmo \
     compact.cmo common.cmo output.cmo outputbis.cmo main.cmo

programs := ocamllex ocamllex.opt

$(foreach program, $(programs), $(eval $(call PROGRAM_SYNONYM,$(program))))

.PHONY: all allopt opt.opt # allopt and opt.opt are synonyms
all: ocamllex
allopt: ocamllex.opt
opt.opt: allopt

ocamllex$(EXE): $(OBJS)
	$(CAMLC) $(LINKFLAGS) -compat-32 -o $@ $^

ocamllex.opt$(EXE): $(OBJS:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -o $@ $^

clean::
	rm -f $(programs) $(programs:=.exe)
	rm -f *.cmo *.cmi *.cmx *.cmt *.cmti *.o *.obj

clean::
	rm -f parser.ml parser.mli parser.output

.PHONY: distclean
distclean: clean

beforedepend:: parser.ml parser.mli

clean::
	rm -f lexer.ml

beforedepend:: lexer.ml

%.cmo: %.ml
	$(CAMLC) -c $(COMPFLAGS) $<

%.cmi: %.mli
	$(CAMLC) -c $(COMPFLAGS) $<

%.cmx: %.ml
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend: beforedepend
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

include .depend
