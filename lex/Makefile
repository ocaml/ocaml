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

# NOTE: it is important that OCAMLLEX is defined *before* Makefile.common
# gets included, so that its definition here takes precedence
# over the one there.
OCAMLLEX ?= $(BOOT_OCAMLLEX)

include $(ROOTDIR)/Makefile.common

OCAMLYACCFLAGS = -v

CAMLC = $(BOOT_OCAMLC) -strict-sequence -nostdlib \
        -I $(ROOTDIR)/boot -use-prims $(ROOTDIR)/runtime/primitives
CAMLOPT = $(OCAMLRUN) $(ROOTDIR)/ocamlopt$(EXE) -nostdlib -I $(ROOTDIR)/stdlib
COMPFLAGS = -absname -w +a-4-9-41-42-44-45-48-70 -warn-error +A \
            -safe-string -strict-sequence -strict-formats -bin-annot
LINKFLAGS =
CAMLDEP = $(BOOT_OCAMLC) -depend
DEPFLAGS = -slash
DEPINCLUDES =

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
	$(CAMLOPT_CMD) -o $@ $^

clean::
	rm -f $(programs) $(programs:=.exe)
	rm -f *.cmo *.cmi *.cmx *.cmt *.cmti *.o *.obj

clean::
	rm -f parser.ml parser.mli parser.output

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
	$(CAMLDEP) $(DEPFLAGS) $(DEPINCLUDES) *.mli *.ml > .depend

include .depend
