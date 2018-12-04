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

-include $(ROOTDIR)/Makefile.config
-include $(ROOTDIR)/Makefile.common

CAMLYACC ?= $(ROOTDIR)/yacc/ocamlyacc

CAMLC = $(BOOT_OCAMLC) -strict-sequence -nostdlib \
        -I $(ROOTDIR)/boot -use-prims $(ROOTDIR)/runtime/primitives
CAMLOPT = $(CAMLRUN) $(ROOTDIR)/ocamlopt -nostdlib -I $(ROOTDIR)/stdlib
COMPFLAGS = -absname -w +a-4-9-41-42-44-45-48 -warn-error A \
            -safe-string -strict-sequence -strict-formats -bin-annot
LINKFLAGS =
YACCFLAGS = -v
CAMLLEX = $(CAMLRUN) $(ROOTDIR)/boot/ocamllex
CAMLDEP = $(BOOT_OCAMLC) -depend
DEPFLAGS = -slash
DEPINCLUDES =

OBJS=cset.cmo syntax.cmo parser.cmo lexer.cmo table.cmo lexgen.cmo \
     compact.cmo common.cmo output.cmo outputbis.cmo main.cmo

.PHONY: all allopt opt.opt # allopt and opt.opt are synonyms
all: ocamllex
allopt: ocamllex.opt
opt.opt: allopt

ocamllex: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -compat-32 -o ocamllex $(OBJS)

ocamllex.opt: $(OBJS:.cmo=.cmx)
	$(CAMLOPT_CMD) -o ocamllex.opt $(OBJS:.cmo=.cmx)

clean::
	rm -f ocamllex ocamllex.opt
	rm -f *.cmo *.cmi *.cmx *.cmt *.cmti *.$(O)

parser.ml parser.mli: parser.mly
	$(CAMLYACC) $(YACCFLAGS) parser.mly

clean::
	rm -f parser.ml parser.mli parser.output

beforedepend:: parser.ml parser.mli

lexer.ml: lexer.mll
	$(CAMLLEX) $(OCAMLLEX_FLAGS) $<

clean::
	rm -f lexer.ml

beforedepend:: lexer.ml

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) $(DEPFLAGS) $(DEPINCLUDES) *.mli *.ml > .depend

include .depend
