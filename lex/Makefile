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
COMPFLAGS = -g -absname -w +a-4-9-42-44-45-48 -warn-error +A \
            -principal -safe-string -strict-sequence -strict-formats -bin-annot
LINKFLAGS = -g

ocamllex_MODULES = cset syntax parser lexer table lexgen compact  \
  common output outputbis main

ocamllex_PROGRAMS := ocamllex ocamllex.opt

$(foreach PROGRAM, $(ocamllex_PROGRAMS),
  $(eval $(call PROGRAM_SYNONYM,$(PROGRAM))))

.PHONY: all allopt opt.opt # allopt and opt.opt are synonyms
all: ocamllex
allopt: ocamllex.opt
opt.opt: allopt

ocamllex$(EXE): $(ocamllex_MODULES:=.cmo)
	$(CAMLC) $(LINKFLAGS) -compat-32 -o $@ $^

ocamllex.opt$(EXE): $(ocamllex_MODULES:=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -o $@ $^

clean::
	rm -f $(ocamllex_PROGRAMS) $(ocamllex_PROGRAMS:=.exe)
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
