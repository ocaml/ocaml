include ../config/Makefile

CAMLRUN=../boot/ocamlrun
CAMLC=$(CAMLRUN) ../boot/ocamlc -I ../boot
CAMLLEX=$(CAMLRUN) ../boot/ocamllex
INCLUDES=-I ../utils -I ../parsing -I ../typing -I ../bytecomp -I ../asmcomp
COMPFLAGS=$(INCLUDES)
LINKFLAGS=$(INCLUDES)

all: ocamldep ocamlprof

# The dependency generator

CAMLDEP=ocamldep.cmo

ocamldep: $(CAMLDEP)
	$(CAMLC) $(LINKFLAGS) -o ocamldep misc.cmo $(CAMLDEP)

clean::
	rm -f ocamldep

ocamldep.ml: ocamldep.mll
	$(CAMLLEX) ocamldep.mll

clean::
	rm -f ocamldep.ml

install::
	cp ocamldep $(BINDIR)/ocamldep

beforedepend:: ocamldep.ml

# The profiler

CSLPROF=ocamlprof.cmo
CSLPROF_IMPORTS=misc.cmo config.cmo clflags.cmo terminfo.cmo \
  linenum.cmo location.cmo longident.cmo pstream.cmo \
  parser.cmo lexer.cmo parse.cmo

ocamlprof: $(CSLPROF) profiling.cmo
	$(CAMLC) $(LINKFLAGS) -o ocamlprof $(CSLPROF_IMPORTS) $(CSLPROF)

install::
	cp ocamlprof $(BINDIR)/ocamlprof
	cp ocamlcp $(BINDIR)/ocamlcp
	cp profiling.cmi profiling.cmo $(LIBDIR)

clean::
	rm -f ocamlprof

# To make custom toplevels

install::
	cp ocamlmktop $(BINDIR)/ocamlmktop

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
	$(CAMLC) $(LINKFLAGS) -o dumpobj misc.cmo tbl.cmo config.cmo ident.cmo opcodes.cmo $(DUMPOBJ)

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
