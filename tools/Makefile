#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id$

include ../config/Makefile

CAMLRUN=../boot/ocamlrun
CAMLC=$(CAMLRUN) ../boot/ocamlc -I ../boot
CAMLLEX=$(CAMLRUN) ../boot/ocamllex
INCLUDES=-I ../utils -I ../parsing -I ../typing -I ../bytecomp -I ../asmcomp \
         -I ../driver
COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES)

all: ocamldep ocamlprof ocamlcp ocamlmktop ocamlmklib

# The dependency generator

CAMLDEP_OBJ=ocamldep.cmo
CAMLDEP_IMPORTS=misc.cmo config.cmo clflags.cmo terminfo.cmo \
  linenum.cmo warnings.cmo location.cmo longident.cmo pstream.cmo \
  syntaxerr.cmo parser.cmo lexer.cmo parse.cmo

ocamldep: $(CAMLDEP_OBJ)
	$(CAMLC) $(LINKFLAGS) -o ocamldep $(CAMLDEP_IMPORTS) $(CAMLDEP_OBJ)

clean::
	rm -f ocamldep

install::
	cp ocamldep $(BINDIR)/ocamldep$(EXE)

# The profiler

CSLPROF=ocamlprof.cmo
CSLPROF_IMPORTS=misc.cmo config.cmo clflags.cmo terminfo.cmo \
  linenum.cmo warnings.cmo location.cmo longident.cmo pstream.cmo \
  syntaxerr.cmo parser.cmo lexer.cmo parse.cmo

ocamlprof: $(CSLPROF) profiling.cmo
	$(CAMLC) $(LINKFLAGS) -o ocamlprof $(CSLPROF_IMPORTS) $(CSLPROF)

ocamlcp: ocamlcp.cmo
	$(CAMLC) $(LINKFLAGS) -o ocamlcp main_args.cmo ocamlcp.cmo

install::
	cp ocamlprof $(BINDIR)/ocamlprof$(EXE)
	cp ocamlcp $(BINDIR)/ocamlcp$(EXE)
	cp profiling.cmi profiling.cmo $(LIBDIR)

clean::
	rm -f ocamlprof ocamlcp

# To make custom toplevels

ocamlmktop: ocamlmktop.tpl ../config/Makefile
	sed -e 's|%%BINDIR%%|$(BINDIR)|' ocamlmktop.tpl > ocamlmktop
	chmod +x ocamlmktop

install::
	cp ocamlmktop $(BINDIR)/ocamlmktop

clean::
	rm -f ocamlmktop

# To help building mixed-mode libraries (Caml + C)

ocamlmklib: ocamlmklib.tpl ../config/Makefile
	sed -e "s|%%BINDIR%%|$(BINDIR)|" \
            -e "s|%%SUPPORTS_SHARED_LIBRARIES%%|$(SUPPORTS_SHARED_LIBRARIES)|" \
            -e "s|%%MKSHAREDLIB%%|$(MKSHAREDLIB)|" \
            -e "s|%%RANLIB%%|$(RANLIB)|" \
          ocamlmklib.tpl > ocamlmklib
	chmod +x ocamlmklib

install::
	cp ocamlmklib $(BINDIR)/ocamlmklib

clean::
	rm -f ocamlmklib

# Converter ocaml 2.04 to 3

OCAML299TO3= lexer299.cmo ocaml299to3.cmo
LIBRARY3= misc.cmo warnings.cmo linenum.cmo location.cmo

ocaml299to3: $(OCAML299TO3)
	$(CAMLC) $(LINKFLAGS) -o ocaml299to3 $(LIBRARY3) $(OCAML299TO3)

lexer299.ml: lexer299.mll
	$(CAMLLEX) lexer299.mll

#install::
#	cp ocaml299to3 $(BINDIR)/ocaml299to3$(EXE)

clean::
	rm -f ocaml299to3 lexer299.ml

# The preprocessor for asm generators

CVT_EMIT=cvt_emit.cmo

cvt_emit: $(CVT_EMIT)
	$(CAMLC) $(LINKFLAGS) -o cvt_emit $(CVT_EMIT)

clean::
	rm -f cvt_emit

cvt_emit.ml: cvt_emit.mll
	$(CAMLLEX) cvt_emit.mll

clean::
	rm -f cvt_emit.ml

beforedepend:: cvt_emit.ml

# The bytecode disassembler

DUMPOBJ=opnames.cmo dumpobj.cmo

dumpobj: $(DUMPOBJ)
	$(CAMLC) $(LINKFLAGS) -o dumpobj \
	         misc.cmo tbl.cmo config.cmo ident.cmo \
	         opcodes.cmo bytesections.cmo $(DUMPOBJ)

clean::
	rm -f dumpobj

opnames.ml: ../byterun/instruct.h
	sed -e '/\/\*/d' \
            -e '/^#/d' \
            -e 's/enum \(.*\) {/let names_of_\1 = [|/' \
            -e 's/};$$/ |]/' \
            -e 's/\([A-Z][A-Z_0-9a-z]*\)/"\1"/g' \
            -e 's/,/;/g' \
        ../byterun/instruct.h > opnames.ml

clean::
	rm -f opnames.ml

beforedepend:: opnames.ml

# Dump .cmx files

dumpapprox: dumpapprox.cmo
	$(CAMLC) $(LINKFLAGS) -o dumpapprox config.cmo dumpapprox.cmo

clean::
	rm -f dumpapprox

# Print imported interfaces for .cmo files

objinfo: objinfo.cmo
	$(CAMLC) $(LINKFLAGS) -o objinfo config.cmo objinfo.cmo

clean::
	rm -f objinfo

# Scan object files for required primitives

PRIMREQ=primreq.cmo

primreq: $(PRIMREQ)
	$(CAMLC) $(LINKFLAGS) -o primreq config.cmo $(PRIMREQ)

clean::
	rm -f primreq

# Common stuff

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

clean::
	rm -f *.cmo *.cmi

depend: beforedepend
	$(CAMLRUN) ./ocamldep $(INCLUDES) *.mli *.ml > .depend

include .depend
